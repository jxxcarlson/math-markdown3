module Editor.View exposing (view)

import Char
import Editor.Config exposing (WrapOption(..))
import Editor.Keymap
import Editor.Model exposing (InternalState)
import Editor.Style as Style
import Editor.Update exposing (Msg(..))
import Editor.Widget as Widget
import Html exposing (Attribute, Html, div, span, text)
import Html.Attributes as Attribute exposing (class, classList, style)
import Html.Events as Event
import Json.Decode as Decode
import Position exposing (Position)
import RollingList
import Window exposing (Window)


name : String
name =
    "elm-editor"


selected : Position -> Maybe Position -> Position -> Bool
selected cursor maybeSelection char =
    maybeSelection
        |> Maybe.map (\selection -> Position.between cursor selection char)
        |> Maybe.withDefault False


{-| The non-breaking space character will not get whitespace-collapsed like a
regular space.
-}
nonBreakingSpace : Char
nonBreakingSpace =
    Char.fromCode 160


ensureNonBreakingSpace : Char -> Char
ensureNonBreakingSpace char =
    if char == ' ' then
        nonBreakingSpace

    else
        char


withTrue : a -> ( a, Bool )
withTrue a =
    ( a, True )


captureOnMouseDown : Msg -> Attribute Msg
captureOnMouseDown msg =
    Event.stopPropagationOn
        "mousedown"
        (Decode.map withTrue (Decode.succeed msg))


captureOnMouseOver : Msg -> Attribute Msg
captureOnMouseOver msg =
    Event.stopPropagationOn
        "mouseover"
        (Decode.map withTrue (Decode.succeed msg))


character : Window -> Position -> Maybe Position -> Position -> Char -> Html Msg
character window cursor selection position char =
    span
        [ classList
            [ ( name ++ "-line__character", True )
            , ( name ++ "-line__character--has-cursor", cursor == Window.shiftPosition_ window position )
            , ( name ++ "-line__character--selected"
              , selected (Window.shiftPosition__ window cursor) (Maybe.map (Window.shiftPosition__ window) selection) position
              )
            ]
        , captureOnMouseDown (MouseDown (Window.shiftPosition_ window position))
        , captureOnMouseOver (MouseOver (Window.shiftPosition_ window position))
        ]
        [ text <| String.fromChar <| ensureNonBreakingSpace char
        , if cursor == Window.shiftPosition_ window position then
            span [ class <| name ++ "-cursor" ] [ text " " ]

          else
            text ""
        ]


line : Window -> Position -> Maybe Position -> Int -> String -> Html Msg
line window cursor selection index content =
    let
        length =
            String.length content

        endPosition =
            { line = index, column = length }

        {- Used below to correctly position and display the cursor -}
        offset =
            window.first
    in
    div
        [ class <| name ++ "-line"
        , captureOnMouseDown (MouseDown endPosition)
        , captureOnMouseOver (MouseOver endPosition)
        ]
    <|
        List.concat
            [ [ span
                    [ class <| name ++ "-line__gutter-padding"
                    , captureOnMouseDown (MouseDown { line = index + 0, column = 0 })
                    , captureOnMouseOver (MouseOver { line = index + 0, column = 0 })
                    ]
                    [ text <| String.fromChar nonBreakingSpace ]
              ]
            , List.indexedMap
                (Window.identity window index >> character window cursor selection)
                (String.toList content)
            , if index == (Window.shiftPosition__ window cursor).line && cursor.column >= length then
                [ span
                    [ class <| name ++ "-line__character"
                    , class <| name ++ "-line__character--has-cursor"
                    ]
                    [ text " "
                    , span [ class <| name ++ "-cursor" ] [ text " " ]
                    ]
                ]

              else
                []
            ]


onTripleClick : msg -> Attribute msg
onTripleClick msg =
    Event.on
        "click"
        (Decode.field "detail" Decode.int
            |> Decode.andThen
                (\detail ->
                    if detail >= 3 then
                        Decode.succeed msg

                    else
                        Decode.fail ""
                )
        )


lineNumber : Int -> Html Msg
lineNumber number =
    span
        [ class <| name ++ "-line-number"
        , captureOnMouseDown (MouseDown { line = number, column = 0 })
        , captureOnMouseOver (MouseOver { line = number, column = 0 })
        ]
        [ text <| String.fromInt (number + 0) ]


gutter : Position -> Window -> Html Msg
gutter cursor window =
    div [ class <| name ++ "-gutter" ] <|
        List.map lineNumber (List.range (window.first + 1) (window.last + 1))


linesContainer : List (Html Msg) -> Html Msg
linesContainer =
    div [ class <| name ++ "-lines" ]


view : List (Attribute Msg) -> List String -> InternalState -> Html Msg
view attr lines state =
    div (attr ++ [ style "position" "absolute" ])
        [ goToLinePanel state
        , searchPanel state
        , infoPanel state lines
        , div
            [ class <| name ++ "-container"
            , Event.preventDefaultOn
                "keydown"
                (Decode.map withTrue Editor.Keymap.decoder)
            , Event.onMouseUp MouseUp
            , Event.onDoubleClick SelectGroup
            , onTripleClick SelectLine
            , Attribute.tabindex 0
            ]
            [ gutter state.cursor state.window
            , linesContainer <|
                List.indexedMap (line state.window state.cursor state.selection) (Window.select state.window lines)
            ]
        ]


infoPanel state lines =
    if state.showInfoPanel then
        infoPanel_ state lines

    else
        div [] []


infoPanel_ state lines =
    div infoPanelStyle
        [ toggleHelpButton state
        , scrollPosition state
        , cursorPosition state
        , lineCount lines
        , wordCount lines
        , wrappingOption state
        , dismissInfoPanel
        ]


wrappingOption state =
    let
        message =
            if state.config.wrapOption == DoWrap then
                "Wrap: ON"

            else
                "Wrap: OFF"
    in
    div [ style "margin-top" "10px" ] [ text message ]


infoPanelStyle =
    [ style "width" "90px"
    , style "position" "absolute"
    , style "right" "8px"
    , style "top" "8px"
    , style "opacity" "1.0"
    , style "border" "solid 0.5px #444"
    , style "background-color" Style.lightBlue
    , style "padding" "8px"
    ]


searchPanel state =
    if state.showSearchPanel == True then
        searchPanel_ state

    else
        div [] []


searchPanel_ state =
    div
        [ style "width" "595px"
        , style "padding-top" "10px"
        , style "height" "36px"
        , style "padding-left" "8px"
        , style "background-color" "#bbb"
        , style "opacity" "0.8"
        , style "font-size" "14px"
        , style "position" "absolute"
        , style "left" "0px"
        , style "top" "0px"
        ]
        [ searchTextButton
        , acceptSearchText
        , numberOfHitsDisplay state
        , showIf (not state.canReplace) openReplaceField
        , showIf state.canReplace replaceTextButton
        , showIf state.canReplace acceptReplaceText
        , searchForwardButton
        , searchBackwardButton
        , dismissSearchPanel
        ]


goToLinePanel state =
    if state.showGoToLinePanel == True then
        goToLinePanel_

    else
        div [] []


goToLinePanel_ =
    div
        [ style "width" "140px"
        , style "height" "36px"
        , style "padding" "ipx"
        , style "opacity" "0.8"
        , style "position" "absolute"
        , style "left" "0px"
        , style "top" "-10px"
        , style "background-color" "#aab"
        ]
        [ goToLineButton
        , acceptLineNumber
        ]


numberOfHitsDisplay : InternalState -> Html Msg
numberOfHitsDisplay state =
    let
        n =
            state.searchResults
                |> RollingList.toList
                |> List.length
                |> String.fromInt
    in
    Widget.rowButton 40 NoOp n []


lineCount : List String -> Html Msg
lineCount lines =
    div Widget.columnButtonStyle [ text ("Lines: " ++ String.fromInt (List.length lines)) ]


wordCount : List String -> Html Msg
wordCount lines =
    let
        words =
            List.map String.words lines |> List.concat
    in
    div Widget.columnButtonStyle [ text ("Words: " ++ String.fromInt (List.length words)) ]


cursorPosition : InternalState -> Html Msg
cursorPosition state =
    div Widget.columnButtonStyle [ text ("Cursor: " ++ String.fromInt (state.cursor.line + 1)) ]


scrollPosition : InternalState -> Html Msg
scrollPosition state =
    div Widget.columnButtonStyle [ text ("Scroll: " ++ String.fromInt state.window.first) ]



-- BUTTONS --


toggleHelpButton state =
    let
        label =
            if state.showHelp == True then
                "Help"

            else
                "Back"
    in
    Widget.columnButton 80 ToggleHelp label []


goToLineButton =
    Widget.rowButton 80
        NoOp
        "Go to line"
        [ style "position" "absolute"
        , style "left" "8px"
        , style "top" "6px"
        ]


dismissInfoPanel =
    Widget.lightColumnButton 20
        ToggleInfoPanel
        "X"
        []


dismissSearchPanel =
    Widget.lightRowButton 25
        ToggleSearchPanel
        "X"
        []


openReplaceField =
    Widget.rowButton 25
        OpenReplaceField
        "R"
        []


searchForwardButton =
    Widget.rowButton 30 RollSearchSelectionForward ">" [ style "float" "left" ]


searchBackwardButton =
    Widget.rowButton 30 RollSearchSelectionBackward "<" [ style "float" "left" ]


searchTextButton =
    Widget.rowButton 60 NoOp "Search" [ style "float" "left" ]


replaceTextButton =
    Widget.rowButton 70 ReplaceCurrentSelection "Replace" [ style "float" "left" ]


acceptLineNumber =
    Widget.textField 30
        AcceptLineNumber
        ""
        [ style "position" "absolute"
        , style "left" "98px"
        , style "top" "6px"
        ]
        [ setHtmlId "line-number-input" ]


acceptSearchText =
    Widget.textField 130 AcceptSearchText "" [ style "float" "left" ] [ setHtmlId "search-box" ]


acceptReplaceText =
    Widget.textField 130 AcceptReplacementText "" [ style "float" "left" ] [ setHtmlId "replacement-box" ]


setHtmlId : String -> Html.Attribute msg
setHtmlId id =
    Attribute.attribute "id" id


showIf : Bool -> Html Msg -> Html Msg
showIf flag h =
    if flag then
        h

    else
        div [] []
