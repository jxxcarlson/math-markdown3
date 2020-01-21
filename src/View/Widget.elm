module View.Widget exposing (footer, modeButtonStrip, searchRow, tabStrip, toolsOrDocs)

import BoundedDeque
import Button
import Config
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentDeleteState(..)
        , DocumentListDisplay
        , DocumentListType(..)
        , EditMode(..)
        , FocusedElement(..)
        , Message
        , MessageType(..)
        , Model
        , Msg(..)
        , SearchMode(..)
        , SearchType(..)
        , SortMode(..)
        , UserState(..)
        , Visibility(..)
        )
import Prng.Uuid as Uuid exposing (Uuid)
import Search
import String.Interpolate exposing (interpolate)
import Style
import TocZ exposing (TocMsg(..), viewZ)
import Utility
import Utility.View
import View.Common exposing (ViewInfo)



-- TAB-STRIP ON LEFT --


tabStrip : ViewInfo -> Model -> Element Msg
tabStrip viewInfo model =
    column [ width (px 30), height (px 200), Background.color (Style.grey 0.1), alignTop ]
        [ row [ spacing 15, rotate -1.5708, moveLeft 50, moveDown 70 ] [ Button.showTools model, Button.showDocumentList model ]
        ]



-- FOOTER --


footer : Model -> Element Msg
footer model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ currentAuthorDisplay model

        -- , el [] (Element.text <| slugOfCurrentDocument model)
        , Button.testButton
        , dirtyDocumentDisplay model
        , wordCount model
        , row [ spacing 4 ] [ Button.totalWordCount, totalWordCountDisplay model ]
        , Utility.View.showIf (Maybe.map .username model.currentUser == Just "jxxcarlson") Button.downloadFile
        , Utility.View.showIf (Maybe.map .username model.currentUser == Just "jxxcarlson") Button.downloadArchive
        , Utility.View.showIf (Maybe.map .username model.currentUser == Just "jxxcarlson") Button.uploadArchive
        , Utility.View.showIf (Maybe.map .username model.currentUser == Just "jxxcarlson") Button.saveImportedArchive
        , Button.shareUrl model
        , shareUrlDisplay model
        , displayMessage model.message

        -- , currentTime model
        ]


currentAuthorDisplay model =
    let
        message =
            case model.currentUser of
                Nothing ->
                    ( UserMessage, "Not signed in" )

                Just user ->
                    ( UserMessage, "Signed in as " ++ user.username )
    in
    Element.el [ Font.color Style.white, Font.size 12 ]
        (Element.text <| messageContent <| message)


messageContent : Message -> String
messageContent ( _, messageContent_ ) =
    messageContent_


shareUrlDisplay : Model -> Element Msg
shareUrlDisplay model =
    case ( model.currentDocument, model.flashCounterForShareUrl > 0 ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, False ) ->
            Element.none

        ( Just doc, True ) ->
            case doc.public of
                True ->
                    -- el [] (Element.text <| Config.endpoint ++ "/#id/" ++ Uuid.toString doc.id)
                    el [] (Element.text <| Config.endpoint ++ "/#doc/" ++ doc.slug)

                False ->
                    el [] (Element.text "Document is private, can't share")


tocCursorDisplay : Model -> Element Msg
tocCursorDisplay model =
    case model.tocCursor of
        Nothing ->
            el [] (Element.text "No focus")

        Just uuid ->
            el [] (Element.text <| "Focus: " ++ Uuid.toString uuid)


dirtyDocumentDisplay : Model -> Element Msg
dirtyDocumentDisplay model =
    case ( model.appMode == Editing StandardEditing, model.currentUser ) of
        ( True, Just _ ) ->
            dirtyDocumentDisplay_ model

        ( _, _ ) ->
            Element.none


dirtyDocumentDisplay_ model =
    if model.currentDocumentDirty then
        row [ width (px 30), height (px 20), Background.color Style.red, Font.color Style.white ]
            [ el [ centerX, centerY ] (Element.text <| String.fromInt model.secondsWhileDirty) ]

    else
        row [ width (px 30), height (px 20), Background.color Style.green, Font.color Style.white ]
            [ el [ centerX, centerY ] (Element.text <| String.fromInt model.secondsWhileDirty) ]


slugOfCurrentDocument : Model -> String
slugOfCurrentDocument model =
    case model.currentDocument of
        Nothing ->
            ""

        Just document ->
            document.slug


wordCount model =
    let
        sourceText =
            case model.currentDocument of
                Just document ->
                    document.content

                _ ->
                    ""

        ( wc, pc ) =
            Utility.pageAndWordCount sourceText
                |> (\( x, y ) -> ( String.fromInt x, String.fromInt y ))

        legend =
            interpolate "Word count {0} ({1} pages)" [ wc, pc ]
    in
    Element.el [ Font.color Style.white, Font.size 12 ] (Element.text <| legend)


totalWordCountDisplay model =
    if model.flashCounterForTotalWordCount == 0 then
        Element.none

    else
        let
            words =
                model.totalWordCount

            pages =
                Basics.round <| toFloat words / Config.wordsPerPage

            t =
                String.fromInt words ++ " (" ++ String.fromInt pages ++ " pages)"
        in
        el [] (Element.text t)


displayMessage : Message -> Element Msg
displayMessage ( messageType, str ) =
    case messageType of
        ErrorMessage ->
            el [ Font.color Style.white, Background.color Style.brightRed, alignRight, Font.size 12, Font.bold, paddingXY 10 4, centerY ] (Element.text str)

        _ ->
            el [ alignRight, paddingXY 10 0 ] (Element.text str)


showToken : Model -> Element Msg
showToken model =
    case model.token of
        Nothing ->
            el [] (Element.text "Token: --")

        Just token ->
            el [] (Element.text <| "Token: " ++ token)


toolsOrDocs viewInfo model =
    case ( model.visibilityOfTools, model.appMode ) of
        ( Visible, Editing StandardEditing ) ->
            toolPanel viewInfo model

        ( _, _ ) ->
            docListViewer viewInfo model


toolPanel viewInfo model =
    let
        h_ =
            View.Common.translate -viewInfo.vInset model.windowHeight
    in
    column
        [ width (px (View.Common.scale (1.35 * viewInfo.docListWidth) model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.5)
        , paddingXY 20 20
        , alignTop
        ]
        [ column [ Font.size 13, spacing 25 ]
            [ el [ Font.size 16, Font.bold, Font.color Style.white ] (Element.text "Document tools")
            , Button.togglePublic model
            , inputTags model
            , docTypePanel model
            , userPermissions model
            ]
        ]


userPermissions model =
    column [ spacing 10, padding 10, Background.color Style.charcoal ]
        [ el [ Font.color Style.white ] (Element.text "User permissions")
        , addUserRow model
        , viewPermissions model
        ]


viewPermissions model =
    case model.currentDocument of
        Nothing ->
            Element.none

        Just doc ->
            let
                permissionList =
                    Document.listPermissions doc
            in
            column [ Font.color Style.white ]
                (List.map (\p -> row [] [ Element.text p ]) permissionList)


addUserRow model =
    row [ spacing 8 ]
        [ Button.addUserPermission, usernameToAddField model, Button.selectPermission model ]


usernameToAddField model =
    Input.text (Style.inputStyle 100)
        { onChange = AddUserNameForPermissions
        , text = model.usernameToAddToPermmission
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, width (px 0) ] (Element.text "")
        }


inputTags model =
    Input.multiline (Style.textInputStyleSimple 180 80)
        { onChange = GotTagString
        , text = model.tagString
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "Keywords")
        , spellcheck = False
        }



-- DOCUMENT LIST --


docListViewer viewInfo model =
    let
        h_ =
            View.Common.translate -viewInfo.vInset model.windowHeight

        renderedList =
            case model.documentListDisplay of
                ( SearchResults, DequeViewOff ) ->
                    renderTocForSearchResults model

                ( DocumentChildren, DequeViewOff ) ->
                    (Button.expandCollapseToc |> Element.map TOC) :: renderTocForMaster model

                ( _, DequeViewOn ) ->
                    renderTocForDeque model
    in
    column
        [ width (px (View.Common.scale viewInfo.docListWidth model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.9)
        , paddingXY 12 20
        , alignTop
        , clipX
        ]
        [ column [ Font.size 13, spacing 8 ]
            (heading model
                :: Button.newSubdocument model
                :: Button.deleteSubdocument model
                :: renderedList
            )
        ]


docTypePanel model =
    let
        w =
            120
    in
    column [ spacing 0 ]
        [ el [ Font.color Style.white, Font.bold, paddingXY 0 8 ] (Element.text "Document Type")
        , Button.miniLaTeX model w
        , Button.extendedMarkdown model w
        , Button.extendedMathMarkdown model w
        ]



-- TABLE OF CONTENTS


renderTocForSearchResults : Model -> List (Element Msg)
renderTocForSearchResults model =
    List.map (tocEntry model.currentDocument) model.documentList


renderTocForDeque : Model -> List (Element Msg)
renderTocForDeque model =
    List.map (tocEntry model.currentDocument)
        (BoundedDeque.toList model.deque |> List.sortBy (\doc -> doc.title))


renderTocForMaster : Model -> List (Element Msg)
renderTocForMaster model =
    case ( model.tocData, model.currentDocument ) of
        ( Nothing, _ ) ->
            [ el [] (Element.text <| "Loading TOC ...") ]

        ( Just zipper, Just doc ) ->
            [ viewZ doc.title model.toggleToc zipper |> Element.map TOC ]

        ( Just zipper, Nothing ) ->
            [ viewZ "__undefined__" model.toggleToc zipper |> Element.map TOC ]


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        ( color, fontWeight ) =
            tocEntryStyle currentDocument_ document
    in
    Input.button []
        { onPress = Just (SetCurrentDocument document)
        , label = el [ Font.color color, fontWeight ] (Element.text <| tocEntryPrefix document ++ document.title)
        }


tocEntryPrefix : Document -> String
tocEntryPrefix doc =
    case List.length doc.childInfo > 0 of
        True ->
            "+ "

        False ->
            ""


tocEntryStyle : Maybe Document -> Document -> ( Color, Element.Attribute msg )
tocEntryStyle currentDocument_ document =
    let
        currentDocId =
            case currentDocument_ of
                Nothing ->
                    Utility.id0

                Just doc ->
                    doc.id

        color =
            case currentDocument_ of
                Nothing ->
                    Style.buttonGrey

                Just _ ->
                    case ( currentDocId == document.id, document.childInfo /= [] ) of
                        ( True, True ) ->
                            Style.brighterBlue

                        ( True, False ) ->
                            Style.red

                        ( False, True ) ->
                            Style.brighterBlue

                        ( False, False ) ->
                            Style.grey 0.2

        fontWeight =
            case currentDocId == document.id of
                True ->
                    Font.bold

                False ->
                    Font.regular
    in
    ( color, fontWeight )



--
-- HEADINGS
--


heading : Model -> Element Msg
heading model =
    let
        n_ =
            case model.documentListDisplay of
                ( SearchResults, DequeViewOff ) ->
                    List.length model.documentList

                ( DocumentChildren, DequeViewOff ) ->
                    List.length model.tableOfContents

                ( _, DequeViewOn ) ->
                    BoundedDeque.length model.deque

        n =
            n_
                |> String.fromInt

        w =
            140
    in
    case model.currentUser of
        Nothing ->
            case model.documentListDisplay of
                ( SearchResults, _ ) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.sortByMostRecentFirst model, Button.sortAlphabetical model ]

                ( DocumentChildren, _ ) ->
                    Input.button []
                        { onPress = Just (SetDocumentListType SearchResults)
                        , label =
                            el (Button.headingStyle w Style.charcoal)
                                (Element.text ("Contents! (" ++ n ++ ")"))
                        }

        Just _ ->
            case model.documentListDisplay of
                ( SearchResults, DequeViewOff ) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.sortByMostRecentFirst model, Button.sortAlphabetical model, Button.setDequeView model ]

                ( SearchResults, DequeViewOn ) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.setDequeView model ]

                ( DocumentChildren, DequeViewOff ) ->
                    row [ spacing 10 ] [ Button.setDocumentChildren model w n, Button.setDequeViewX model w n ]

                ( DocumentChildren, DequeViewOn ) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.setDequeViewX model w n ]


modeButtonStrip model lhWidth =
    row [ width (px lhWidth), height (px 45), spacing 10, paddingXY 20 0 ]
        [ editTools model
        , Button.readingMode model
        , Button.userPageMode model
        ]


searchRow model =
    row [ spacing 10, alignRight ] [ Search.inputTerms model, Button.clearSearchTerms, Button.search model, Button.allDocuments, Button.helpDocs ]


editTools : Model -> Element Msg
editTools model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            if List.member model.appMode [ Editing StandardEditing, Editing SubdocumentEditing ] then
                row [ spacing 6, width (px 360) ]
                    [ Button.editingMode model
                    , Button.subDocumentEditingMode model
                    , Button.newDocument model
                    , Button.firstSubDocument model
                    , Button.saveDocument model
                    , Button.deleteDocument model
                    , Button.cancelDeleteDocumentInHeader model
                    ]

            else
                row [ spacing 6 ] [ Button.editingMode model, Button.subDocumentEditingMode model ]
