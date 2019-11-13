module Button exposing
    ( addSubdocument2
    , addUserPermission
    , allDocuments
    , cancelChangePassword
    , cancelDeleteDocumentInHeader
    , cancelSignUp
    , changePassword
    , clearSearchTerms
    , deleteDocument
    , deleteSubdocument
    , editingMode
    , expandCollapseToc
    , extendedMarkdown
    , extendedMathMarkdown
    , firstSubDocument
    , getTextSelection
    , headingStyle
    , helpDocs
    , miniLaTeX
    , newDocument
    , newSubdocument
    , readingMode
    , saveDocument
    , search
    , selectPermission
    , setDequeView
    , setDequeViewX
    , setDocumentChildren
    , setDocumentListType
    , setupOutline
    , shareUrl
    , showDocumentList
    , showTools
    , signIn
    , signOut
    , signUp
    , sortAlphabetical
    , sortByMostRecentFirst
    , subDocumentEditingMode
    , togglePublic
    , totalWordCount
    , updateChildren
    , userPageMode
    )

import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Element exposing (Color, Element, centerX, el, height, moveDown, padding, paddingXY, px, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentDeleteState(..)
        , DocumentListType(..)
        , EditMode(..)
        , Model
        , Msg(..)
        , SearchMode(..)
        , SortMode(..)
        , UserState(..)
        , Visibility(..)
        )
import Style
import TocZ exposing (TocMsg(..))
import Utility.View


togglePublic model =
    case model.currentDocument of
        Nothing ->
            Element.none

        Just document ->
            case document.public of
                True ->
                    Input.button []
                        { onPress = Just (SetDocumentPublic False)
                        , label = el (headingStyle 140 Style.charcoal) (Element.text "Public")
                        }

                False ->
                    Input.button []
                        { onPress = Just (SetDocumentPublic True)
                        , label = el (headingStyle 140 Style.charcoal) (Element.text "Private")
                        }


selectPermission model =
    let
        labelText =
            case model.permissionToAdd of
                NoPermission ->
                    "N"

                ReadPermission ->
                    "R"

                WritePermission ->
                    "W"
    in
    Input.button (headingStyle 30 Style.charcoal ++ [ Border.color Style.white, Border.width 1 ])
        { onPress = Just CyclePermission
        , label = el [ Font.color Style.white ] (Element.text labelText)
        }



-- MODE BUTTONS


editingMode model =
    let
        color =
            if model.appMode == Editing StandardEditing then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode (Editing StandardEditing))
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "Edit"))
        }


readingMode model =
    let
        color =
            if model.appMode == Reading then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode Reading)
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "Read"))
        }


userPageMode model =
    let
        color =
            case model.appMode of
                UserMode _ ->
                    Style.red

                _ ->
                    Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode (UserMode SignedInState))
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "User"))
        }


subDocumentEditingMode model =
    let
        color =
            if model.appMode == Editing SubdocumentEditing then
                Style.red

            else
                Style.buttonGrey
    in
    Utility.View.showIf
        (model.currentUser
            /= Nothing
            && List.length model.tableOfContents
            > 0
            && List.member model.documentListDisplay [ ( DocumentChildren, DequeViewOff ), ( DocumentChildren, DequeViewOff ) ]
        )
        (Input.button []
            { onPress = Just (SetAppMode (Editing SubdocumentEditing))
            , label =
                el (headerButtonStyle color)
                    (el headerLabelStyle (Element.text "Edit/S"))
            }
        )



-- AAA


showTools : Model -> Element Msg
showTools model =
    let
        color =
            if model.visibilityOfTools == Visible then
                Style.red

            else
                Style.buttonGrey
    in
    case model.appMode of
        Editing StandardEditing ->
            Input.button []
                { onPress = Just (SetToolPanelState Visible)
                , label = el [ height (px 30), width (px 50), padding 8, Background.color color, Font.color Style.white, Font.size 12 ] (Element.text "Tools")
                }

        _ ->
            Input.button []
                { onPress = Just NoOp
                , label = el [ height (px 30), width (px 50), padding 8, Background.color Style.charcoal, Font.color Style.white, Font.size 12 ] (Element.text "")
                }


showDocumentList : { a | visibilityOfTools : Visibility } -> Element Msg
showDocumentList model =
    let
        color =
            if model.visibilityOfTools == Invisible then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetToolPanelState Invisible)
        , label = el [ height (px 30), padding 8, Background.color color, Font.color Style.white, Font.size 12 ] (Element.text "Documents")
        }


search : Model -> Element Msg
search model =
    let
        title =
            case model.searchMode of
                UserSearch ->
                    "My docs"

                PublicSearch ->
                    "Public docs"

                SharedDocSearch ->
                    "Shared docs"
    in
    Input.button []
        { onPress = Just ToggleSearchMode
        , label =
            el [ height (px 30), width (px 75), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text title))
        }


sortAlphabetical : Model -> Element Msg
sortAlphabetical model =
    let
        color =
            case model.sortMode == Alphabetical of
                True ->
                    Style.darkRed

                False ->
                    Style.charcoal
    in
    Input.button (Style.standardButton ++ [ Background.color color, Font.color Style.white ])
        { onPress = Just (SetSortMode Alphabetical)
        , label = el [] (Element.text "A")
        }


sortByMostRecentFirst : Model -> Element Msg
sortByMostRecentFirst model =
    let
        color =
            case model.sortMode == MostRecentFirst of
                True ->
                    Style.darkRed

                False ->
                    Style.charcoal
    in
    Input.button (Style.standardButton ++ [ Background.color color, Font.color Style.white ])
        { onPress = Just (SetSortMode MostRecentFirst)
        , label = el [] (Element.text "R")
        }


allDocuments : Element Msg
allDocuments =
    Input.button []
        { onPress = Just AllDocuments
        , label =
            el [ height (px 30), width (px 40), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text "All"))
        }


helpDocs : Element Msg
helpDocs =
    Input.button []
        { onPress = Just GetHelpDocs
        , label =
            el [ height (px 30), width (px 40), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text "Help"))
        }


clearSearchTerms : Element Msg
clearSearchTerms =
    Input.button []
        { onPress = Just ClearSearchTerms
        , label =
            el [ height (px 30), width (px 25), centerX, padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2 ] (Element.text "X"))
        }



--- AAA


shareUrl model =
    Input.button []
        { onPress = Just DoShareUrl
        , label = el [] (Element.text "Share: ")
        }


getTextSelection =
    Input.button []
        { onPress = Just GetTextSelection
        , label = el [] (Element.text "Sync L <- R")
        }


totalWordCount =
    Input.button []
        { onPress = Just DoTotalWordCount
        , label = el [] (Element.text "Total word count: ")
        }



---- BUTTONS --


miniLaTeX model width =
    let
        bit =
            model.docType == MiniLaTeX
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType MiniLaTeX), label = el [ paddingXY 8 0 ] (Element.text "MiniLaTeX") }


standardMarkdownButton model width =
    let
        bit =
            model.docType == Markdown MDStandard
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDStandard)), label = el [ paddingXY 8 0 ] (Element.text "Markdown standard") }


extendedMarkdown model width =
    let
        bit =
            model.docType == Markdown MDExtended
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDExtended)), label = el [ paddingXY 8 0 ] (Element.text "Markdown") }


extendedMathMarkdown model width =
    let
        bit =
            model.docType == Markdown MDExtendedMath
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDExtendedMath)), label = el [ paddingXY 8 0 ] (Element.text "Markdown + math") }



--- DOCUMENT


newDocument model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just CreateDocument
                , label = el toolButtonStyleInHeader (Element.text "New")
                }


firstSubDocument model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just FirstSubdocument
                , label = el toolButtonStyleInHeader (Element.text "First/S")
                }


saveDocument model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just SaveDocument
                , label = el toolButtonStyleInHeader (Element.text "Save")
                }


deleteDocument model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            case model.documentDeleteState of
                Armed ->
                    Input.button []
                        { onPress = Just DeleteDocument
                        , label = el (toolButtonStyleInHeader ++ [ Background.color Style.red ]) (Element.text "Delete!")
                        }

                SafetyOn ->
                    Input.button []
                        { onPress = Just ArmForDelete
                        , label = el toolButtonStyleInHeader (Element.text "Delete?")
                        }


cancelDeleteDocumentInHeader model =
    case model.documentDeleteState of
        Armed ->
            Input.button []
                { onPress = Just CancelDeleteDocument
                , label = el (toolButtonStyleInHeader ++ [ Background.color Style.blue ]) (Element.text "Cancel")
                }

        SafetyOn ->
            Element.none


deleteSubdocument : Model -> Element Msg
deleteSubdocument model =
    let
        numberOfChildren =
            Maybe.map (.childInfo >> List.length) model.currentDocument
                |> Maybe.withDefault 0
    in
    Utility.View.showIf (model.appMode == Editing SubdocumentEditing)
        (Input.button
            []
            { onPress = Just DeleteSubdocument
            , label =
                el []
                    (el (headingStyle 140 Style.charcoal) (Element.text "Delete subdocument"))
            }
        )


newSubdocument model =
    let
        numberOfChildren =
            Maybe.map (.childInfo >> List.length) model.currentDocument
                |> Maybe.withDefault 0
    in
    Utility.View.showIf (model.appMode == Editing SubdocumentEditing)
        (Input.button
            []
            { onPress = Just NewSubdocument
            , label =
                el []
                    (el (headingStyle 140 Style.charcoal) (Element.text "New subdocument"))
            }
        )


expandCollapseToc =
    Input.button (Style.activeButtonStyle ++ [ Font.size 12 ])
        { onPress = Just Toggle
        , label = Element.text "Expand/Collapse"
        }



-- AAA


setDequeViewX model w n =
    let
        color =
            case model.documentListDisplay of
                ( _, DequeViewOn ) ->
                    Style.darkBlue

                ( _, DequeViewOff ) ->
                    Style.charcoal
    in
    Input.button []
        { onPress = Just ToggleDequeview
        , label =
            el (headingStyle w color)
                (Element.text "Recent")
        }


setDocumentChildren model w n =
    let
        color =
            case model.documentListDisplay of
                ( _, DequeViewOff ) ->
                    Style.darkBlue

                ( _, DequeViewOn ) ->
                    Style.charcoal
    in
    Input.button []
        { onPress = Just (SetDocumentListType SearchResults)
        , label =
            el (headingStyle w color)
                (Element.text ("Contents (" ++ n ++ ")"))
        }


setDocumentListType model w n =
    let
        msg =
            case Maybe.map .childInfo model.currentDocument == Just [] of
                True ->
                    ToggleDequeview

                False ->
                    SetDocumentListType DocumentChildren

        color =
            case model.documentListDisplay of
                ( _, DequeViewOff ) ->
                    Style.darkBlue

                ( _, DequeViewOn ) ->
                    Style.charcoal
    in
    Input.button []
        { onPress = Just msg
        , label =
            el (headingStyle w color)
                (Element.text ("Documents (" ++ n ++ ")"))
        }


setDequeView model =
    let
        color =
            if Tuple.second model.documentListDisplay == DequeViewOn then
                Style.darkBlue

            else
                Style.charcoal
    in
    Input.button []
        { onPress = Just ToggleDequeview
        , label =
            el (headingStyle 60 color)
                (Element.text "Recent")
        }



-- AAA


setupOutline model =
    Input.button [] { onPress = Just SetUpOutline, label = el xButtonStyle (Element.text "Load") }


updateChildren model =
    Input.button [] { onPress = Just UpdateChildren, label = el xButtonStyle (Element.text "Update") }


addSubdocument2 : Document -> Element Msg
addSubdocument2 document =
    Input.button Style.activeButtonStyle { onPress = Just (AddThisDocumentToMaster document), label = el [ Font.color Style.white ] (Element.text document.title) }


xButtonStyle =
    Style.standardButton ++ [ Background.color Style.charcoal, Font.color Style.white ]



-- USER signin


addUserPermission =
    Input.button (headingStyle 40 Style.charcoal ++ [ Border.color Style.white, Border.width 1 ])
        { onPress = Just AddUserPermission
        , label = el [] (Element.text "Add")
        }


changePassword : Model -> Element Msg
changePassword model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode ChangePasswordState ->
                    Just ChangePassword

                _ ->
                    Just <| SetAppMode (UserMode ChangePasswordState)
        , label = Element.text "Change password"
        }


signIn : Element Msg
signIn =
    Input.button Style.standardButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUp : Model -> Element Msg
signUp model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode SignUpState ->
                    Just SignUp

                _ ->
                    Just (SetAppMode (UserMode SignUpState))
        , label = Element.text "Sign Up!!"
        }


cancelSignUp : Model -> Element Msg
cancelSignUp model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode SignUpState ->
                    Just (SetAppMode (UserMode SignInState))

                _ ->
                    Just NoOp
        , label = Element.text "Cancel"
        }


cancelChangePassword : Model -> Element Msg
cancelChangePassword model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode ChangePasswordState ->
                    Just (SetAppMode (UserMode SignInState))

                _ ->
                    Just NoOp
        , label = Element.text "Cancel"
        }


signOut : Model -> Element Msg
signOut model =
    Input.button Style.standardButton
        { onPress = Just SignOut
        , label = Element.text "Sign out"
        }



-- HELPERS


headerButtonStyle color =
    [ height (px 30), width (px 50), Background.color color, Font.color Style.white, Font.size 12 ]


headerLabelStyle =
    [ height (px 30), width (px 80), padding 8 ]


headingStyle : Int -> Color -> List (Element.Attribute msg)
headingStyle w color =
    [ height (px 30), width (px w), padding 8, Background.color color, Font.color Style.white, Font.size 12 ]


toolButtonStyleInHeader : List (Element.Attribute msg)
toolButtonStyleInHeader =
    [ height (px 30), width (px 60), padding 8, Background.color (Style.makeGrey 0.1), Border.color Style.white, Font.color Style.white, Font.size 12 ]
