module Button exposing
    ( allDocuments
    , clearSearchTerms
    , editingMode
    , extendedMarkdown
    , extendedMathMarkdown
    , getTextSelection
    , headingStyle
    , helpDocs
    , miniLaTeX
    , newSubdocument
    , readingMode
    , search
    , shareUrl
    , showDocumentList
    , showTools
    , sortAlphabetical
    , sortByMostRecentFirst
    , subDocumentEditingMode
    , totalWordCount
    , userPageMode
    )

import Document exposing (DocType(..), MarkdownFlavor(..))
import Element exposing (Color, Element, centerX, el, height, moveDown, padding, paddingXY, px, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
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
import Utility.View



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



-- HELPERS


headerButtonStyle color =
    [ height (px 30), width (px 50), Background.color color, Font.color Style.white, Font.size 12 ]


headerLabelStyle =
    [ height (px 30), width (px 80), padding 8 ]


headingStyle : Int -> Color -> List (Element.Attribute msg)
headingStyle w color =
    [ height (px 30), width (px w), padding 8, Background.color color, Font.color Style.white, Font.size 12 ]
