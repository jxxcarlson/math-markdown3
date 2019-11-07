module Button exposing
    ( editingMode
    , readingMode
    , subDocumentEditingMode
    , userPageMode
    )

import Element exposing (el, height, padding, px, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentListType(..)
        , EditMode(..)
        , Msg(..)
        , UserState(..)
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



-- HELPERS


headerButtonStyle color =
    [ height (px 30), width (px 50), Background.color color, Font.color Style.white, Font.size 12 ]


headerLabelStyle =
    [ height (px 30), width (px 80), padding 8 ]
