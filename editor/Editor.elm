module Editor exposing
    ( Editor
    , EditorConfig
    , EditorMsg
    , embedded
    , getCursor
    , getSelectedText
    , getSource
    , getWrapOption
    , init
    , insert
    , load
    , pasteFromClipBoard
    , placeInClipboard
    , scrollToLine
    , scrollToString
    , setSelectedText
    , slider
    , sliderUpdate
    , sliderView
    , update
    , updateSlider
    , view
    )

import Buffer exposing (Buffer)
import Editor.Config exposing (WrapOption(..), WrapParams)
import Editor.History
import Editor.Model exposing (InternalState)
import Editor.Styles
import Editor.Update
import Editor.View
import Editor.Wrap
import Html exposing (Attribute, Html, div)
import Html.Attributes as HA exposing (style)
import Position exposing (Position)
import RollingList
import SingleSlider as Slider


type alias EditorMsg =
    Editor.Update.Msg


type Editor
    = Editor
        { buffer : Buffer
        , state : InternalState
        }



-- GETTERS --


getWrapOption : Editor -> WrapOption
getWrapOption (Editor data) =
    data.state.config.wrapOption


getSource : Editor -> String
getSource (Editor data) =
    Buffer.toString data.buffer


getCursor : Editor -> Position
getCursor (Editor data) =
    data.state.cursor


getSelectedText : Editor -> Maybe String
getSelectedText (Editor data) =
    data.state.selectedText


getSmallConfig : InternalState -> SmallEditorConfig
getSmallConfig s =
    s.config



-- SETTERS --


setSelectedText : String -> Editor -> Editor
setSelectedText str (Editor data) =
    let
        is =
            data.state
    in
    Editor { data | state = { is | selectedText = Just str } }



-- CONFIG --


type alias EditorConfig a =
    { editorMsg : EditorMsg -> a
    , sliderMsg : Slider.Msg -> a
    , editorStyle : List (Html.Attribute a)
    , width : Int
    , lines : Int
    , lineHeight : Float
    , showInfoPanel : Bool
    , wrapParams : { maximumWidth : Int, optimalWidth : Int, stringWidth : String -> Int }
    , wrapOption : WrapOption
    }


type alias SmallEditorConfig =
    { lines : Int
    , showInfoPanel : Bool
    , wrapParams : { maximumWidth : Int, optimalWidth : Int, stringWidth : String -> Int }
    , wrapOption : WrapOption
    }


smallConfig : EditorConfig a -> SmallEditorConfig
smallConfig c =
    { lines = c.lines
    , showInfoPanel = c.showInfoPanel
    , wrapParams = c.wrapParams
    , wrapOption = c.wrapOption
    }



-- EMBEDDED EDITOR --


embedded : EditorConfig a -> Editor -> Html a
embedded editorConfig editor =
    div [ style "position" "absolute" ]
        [ div editorConfig.editorStyle
            [ Editor.Styles.styles { width = editorConfig.width, lineHeight = editorConfig.lineHeight, numberOfLines = editorConfig.lines }
            , view [ style "background-color" "#eeeeee" ] editor
                |> Html.map editorConfig.editorMsg
            , div [ HA.style "position" "absolute" ]
                [ sliderView editor |> Html.map editorConfig.sliderMsg ]
            ]
        ]


init : EditorConfig a -> String -> Editor
init editorConfig text =
    Editor
        { buffer = Buffer.init text
        , state =
            { config = smallConfig editorConfig
            , scrolledLine = 0
            , cursor = Position 0 0
            , window = { first = 0, last = editorConfig.lines - 1 }
            , selection = Nothing
            , selectedText = Nothing
            , clipboard = ""
            , dragging = False
            , history = Editor.History.empty
            , searchTerm = ""
            , replacementText = ""
            , canReplace = False
            , searchResults = RollingList.fromList []
            , showHelp = True
            , showInfoPanel = editorConfig.showInfoPanel
            , showGoToLinePanel = False
            , showSearchPanel = False
            , savedBuffer = Buffer.fromString ""
            , slider = Editor.Model.slider
            }
        }



-- UPDATE --


update : EditorMsg -> Editor -> ( Editor, Cmd EditorMsg )
update msg (Editor data) =
    let
        ( is, b, cmd ) =
            Editor.Update.update data.buffer msg data.state
    in
    ( Editor { state = is, buffer = b }, cmd )


sliderUpdate : Slider.Msg -> Editor -> ( Editor, Cmd Slider.Msg )
sliderUpdate sliderMsg ((Editor data) as editor) =
    let
        ( newSlider, cmd, updateResults ) =
            Slider.update sliderMsg (slider editor)

        newEditor =
            updateSlider newSlider editor

        numberOfLines =
            Buffer.lines data.buffer
                |> List.length
                |> toFloat

        line =
            newSlider.value
                / 100.0
                |> (\x -> x * numberOfLines)
                |> round

        newCmd =
            if updateResults then
                cmd

            else
                Cmd.none
    in
    ( scrollToLine line newEditor, newCmd )



-- VIEW --


view : List (Attribute EditorMsg) -> Editor -> Html EditorMsg
view attr (Editor data) =
    Editor.View.view attr (Buffer.lines data.buffer) data.state



-- SLIDER --


slider : Editor -> Slider.Model
slider (Editor data) =
    data.state.slider


updateSlider : Slider.Model -> Editor -> Editor
updateSlider slider_ (Editor data) =
    let
        oldState =
            data.state
    in
    Editor { data | state = { oldState | slider = slider_ } }


sliderView : Editor -> Html Slider.Msg
sliderView (Editor data) =
    Html.div
        [ style "position" "absolute", style "right" "0px", style "top" "0px" ]
        [ Slider.view data.state.slider ]



--  EDITOR FUNCTIONS --


wrapSelection : Editor -> Editor
wrapSelection ((Editor data) as editor) =
    case data.state.selection of
        Nothing ->
            editor

        Just sel ->
            let
                ( start, end ) =
                    Position.order sel data.state.cursor

                selectedText =
                    Buffer.between start end data.buffer

                wrappedText =
                    Editor.Wrap.paragraphs data.state.config selectedText

                oldState =
                    data.state

                newState =
                    { oldState | selectedText = Just selectedText }

                newBuffer =
                    Buffer.replace start end wrappedText data.buffer
            in
            Editor { state = newState, buffer = newBuffer }



-- ?? -- |> recordHistory state buffer


insert : WrapOption -> Position -> String -> Editor -> Editor
insert wrapOption position string (Editor data) =
    let
        textToInsert =
            case wrapOption of
                DoWrap ->
                    Editor.Wrap.paragraphs data.state.config string

                DontWrap ->
                    string
    in
    Editor { data | buffer = Buffer.insert position textToInsert data.buffer }


placeInClipboard : String -> Editor -> Editor
placeInClipboard str (Editor data) =
    let
        oldState =
            data.state

        newState =
            { oldState | clipboard = str }
    in
    Editor { data | state = newState }


pasteFromClipBoard : Editor -> Editor
pasteFromClipBoard (Editor data) =
    Editor { data | buffer = Buffer.insert data.state.cursor data.state.clipboard data.buffer }


clearState : Editor -> Editor
clearState (Editor data) =
    Editor { data | state = Editor.Update.clearState data.state }


load : WrapOption -> String -> Editor -> Editor
load wrapOption content ((Editor data) as editor) =
    let
        config =
            data.state.config

        lineLengths =
            String.lines content |> List.map String.length

        maxLineLength =
            List.maximum lineLengths |> Maybe.withDefault 1000

        buffer =
            if wrapOption == DoWrap && maxLineLength > config.wrapParams.maximumWidth then
                Buffer.fromString (Editor.Wrap.paragraphs config content)

            else
                Buffer.fromString content

        (Editor newData) =
            clearState editor
    in
    Editor { newData | buffer = buffer }


scrollToString : String -> Editor -> Editor
scrollToString str (Editor data) =
    let
        ( is, b ) =
            Editor.Update.scrollToText str data.state data.buffer
    in
    Editor { state = is, buffer = b }


scrollToLine : Int -> Editor -> Editor
scrollToLine k (Editor data) =
    let
        ( is, b ) =
            Editor.Update.scrollToLine k data.state data.buffer
    in
    Editor { state = is, buffer = b }
