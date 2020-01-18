module Editor.Update exposing (Msg(..), blur, clearState, focus, scrollToLine, scrollToText, update)

{-| Blah, blah ...

@docs Msg, blur, clearState, focus, scrollToLine, scrollToText, update

-}

import Browser.Dom as Dom
import Buffer exposing (Buffer)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Editor.Config as Config exposing (Config, WrapOption(..))
import Editor.History
import Editor.Model exposing (InternalState, Snapshot)
import Editor.Search
import Editor.Strings
import Editor.Wrap
import Position exposing (Position)
import RollingList
import Task exposing (Task)
import Window



-- MSG


{-| The messages to which the editor responds
-}
type Msg
    = NoOp
    | MouseDown Position
    | MouseOver Position
    | MouseUp
    | Copy
    | CopyPasteClipboard
    | WriteToSystemClipBoard
    | Cut
    | CursorLeft
    | CursorRight
    | CursorUp
    | CursorDown
    | CursorToLineEnd
    | CursorToLineStart
    | CursorToGroupEnd
    | CursorToGroupStart
    | Insert String
    | FirstLine
    | AcceptLineNumber String
    | AcceptSearchText String
    | AcceptReplacementText String
    | ReplaceCurrentSelection
    | LastLine
    | Paste
    | PasteFromClipboard
    | RemoveCharAfter
    | RemoveCharBefore
    | RemoveGroupAfter
    | RemoveGroupBefore
    | Indent
    | Deindent
    | SelectUp
    | SelectDown
    | SelectLeft
    | SelectRight
    | SelectToLineStart
    | SelectToLineEnd
    | SelectToGroupStart
    | SelectToGroupEnd
    | SelectAll
    | SelectGroup
    | SelectLine
    | SendLine
    | Undo
    | Redo
    | ScrollUp Int
    | ScrollDown Int
    | ScrollToSelection ( Position, Position )
    | RollSearchSelectionForward
    | RollSearchSelectionBackward
    | Clear
    | WrapSelection
    | WrapAll
    | ToggleWrapping
    | ToggleHelp
    | ToggleInfoPanel
    | ToggleGoToLinePanel
    | ToggleSearchPanel
    | ToggleReplacePanel
    | OpenReplaceField
    | DebounceMsg Debounce.Msg
    | Unload String


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 100
    , transform = DebounceMsg
    }


autoclose : Dict String String
autoclose =
    Dict.fromList
        [ ( "[", "]" )
        , ( "{", "}" )
        , ( "(", ")" )
        , ( "\"", "\"" )
        , ( "'", "'" )
        , ( "`", "`" )
        ]


stateToSnapshot : InternalState -> Buffer -> Snapshot
stateToSnapshot { cursor, selection } buffer =
    { cursor = cursor, selection = selection, buffer = buffer }


recordHistory :
    InternalState
    -> Buffer
    -> ( InternalState, Buffer, Cmd Msg )
    -> ( InternalState, Buffer, Cmd Msg )
recordHistory oldState oldBuffer ( state, buffer, cmd ) =
    ( { state
        | history =
            if oldBuffer /= buffer then
                Editor.History.push
                    (stateToSnapshot oldState oldBuffer)
                    state.history

            else
                state.history
      }
    , buffer
    , cmd
    )


{-| Return data representing an editor updated via a message and a new buffer
-}
update : Buffer -> Msg -> InternalState -> ( InternalState, Buffer, Cmd Msg )
update buffer msg state =
    case msg of
        NoOp ->
            ( state, buffer, Cmd.none )

        MouseDown position ->
            ( { state
                | cursor = position
                , dragging = True
                , selection = Nothing
              }
            , buffer
            , Cmd.none
            )

        MouseOver position ->
            if state.dragging then
                ( { state
                    | selection =
                        case state.selection of
                            Just selection ->
                                if selection == position then
                                    Nothing

                                else
                                    Just selection

                            Nothing ->
                                if position == state.cursor then
                                    Nothing

                                else
                                    Just state.cursor
                    , cursor = position
                  }
                , buffer
                , Cmd.none
                )

            else
                ( state, buffer, Cmd.none )

        MouseUp ->
            ( { state | dragging = False }, buffer, Cmd.none )

        CursorLeft ->
            let
                newCursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.first

                                Nothing ->
                                    state.cursor
                    in
                    Position.previousColumn moveFrom
                        |> Buffer.clampPosition Buffer.Backward buffer

                cmd =
                    case state.cursor.line /= newCursor.line of
                        True ->
                            setEditorViewportForLine newCursor.line

                        False ->
                            Cmd.none
            in
            ( { state
                | cursor = newCursor
                , window = Window.scrollToIncludeCursor newCursor state.window
                , selection = Nothing
              }
            , buffer
            , cmd
            )

        CursorRight ->
            let
                newCursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.second

                                Nothing ->
                                    state.cursor
                    in
                    Position.nextColumn moveFrom
                        |> Buffer.clampPosition Buffer.Forward buffer

                cmd =
                    case state.cursor.line /= newCursor.line of
                        True ->
                            setEditorViewportForLine newCursor.line

                        False ->
                            Cmd.none
            in
            ( { state
                | cursor = newCursor
                , window = Window.scrollToIncludeCursor newCursor state.window
                , selection = Nothing
              }
            , buffer
            , cmd
            )

        CursorUp ->
            let
                newCursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.first

                                Nothing ->
                                    state.cursor
                    in
                    Position.previousLine moveFrom
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
            ( { state
                | cursor = newCursor
                , selection = Nothing
              }
            , buffer
            , setEditorViewportForLine newCursor.line
            )

        CursorDown ->
            let
                newCursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.second

                                Nothing ->
                                    state.cursor
                    in
                    Position.nextLine moveFrom
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
            ( { state
                | cursor = newCursor
                , selection = Nothing
              }
            , buffer
            , setEditorViewportForLine newCursor.line
            )

        CursorToLineEnd ->
            ( { state
                | cursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.second

                                Nothing ->
                                    state.cursor
                    in
                    case Buffer.lineEnd moveFrom.line buffer of
                        Just column ->
                            Position.setColumn column state.cursor

                        Nothing ->
                            Buffer.clampPosition
                                Buffer.Backward
                                buffer
                                state.cursor
                , selection = Nothing
              }
            , buffer
            , Cmd.none
            )

        CursorToLineStart ->
            ( { state
                | cursor =
                    let
                        moveFrom =
                            case state.selection of
                                Just selection ->
                                    Position.order selection state.cursor
                                        |> Tuple.first

                                Nothing ->
                                    state.cursor
                    in
                    Position.setColumn 0 moveFrom
                , selection = Nothing
              }
            , buffer
            , Cmd.none
            )

        CursorToGroupEnd ->
            ( { state
                | cursor = Buffer.groupEnd state.cursor buffer
                , selection = Nothing
              }
            , buffer
            , Cmd.none
            )

        CursorToGroupStart ->
            ( { state
                | cursor = Buffer.groupStart state.cursor buffer
                , selection = Nothing
              }
            , buffer
            , Cmd.none
            )

        Paste ->
            case state.selectedText of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just text ->
                    ( state, Buffer.insert state.cursor text buffer, Cmd.none )

        PasteFromClipboard ->
            ( state, Buffer.insert state.cursor state.clipboard buffer, Cmd.none )

        CopyPasteClipboard ->
            {- The msg CopyPasteClipboard is detected and acted upon by the
               host app's update function.
            -}
            ( state, buffer, Cmd.none )

        WriteToSystemClipBoard ->
            {- The msg WriteToSystemClipBoard is detected and acted upon by the
               host app's update function.
            -}
            case state.selection of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just sel ->
                    let
                        ( start, end ) =
                            Position.order sel state.cursor

                        selectedText =
                            Buffer.between start end buffer

                        newState =
                            { state | selectedText = Just selectedText }
                    in
                    ( newState, buffer, Cmd.none )
                        |> recordHistory state buffer

        Insert string ->
            case ( state.selection, Dict.get string autoclose ) of
                ( Just selection, Just closing ) ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor

                        wrapped =
                            string
                                ++ Buffer.between start end buffer
                                ++ closing
                    in
                    ( { state
                        | cursor =
                            if state.cursor.line == start.line then
                                Position.nextColumn state.cursor

                            else
                                state.cursor
                        , selection =
                            Just <|
                                if selection.line == start.line then
                                    Position.nextColumn selection

                                else
                                    selection
                      }
                    , Buffer.replace start end wrapped buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                ( Just selection, Nothing ) ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                    ( { state
                        | cursor =
                            if string == "\n" then
                                { line = start.line + 1
                                , column = 0
                                }

                            else
                                Position.nextColumn start
                        , selection = Nothing
                      }
                    , Buffer.replace start end string buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                ( Nothing, maybeClosing ) ->
                    let
                        nearWordChar =
                            Buffer.nearWordChar state.cursor buffer

                        insertString =
                            if not nearWordChar then
                                Maybe.map ((++) string) maybeClosing
                                    |> Maybe.withDefault string

                            else
                                string

                        ( debounce, debounceCmd ) =
                            Debounce.push debounceConfig insertString state.debounce
                    in
                    let
                        ( newCursor, scrollCmd ) =
                            if string == "\n" then
                                ( { line = state.cursor.line + 1, column = 0 }, setEditorViewportForLine (state.cursor.line + 1) )

                            else
                                ( Position.nextColumn state.cursor, Cmd.none )
                    in
                    ( { state
                        | debounce = debounce
                        , cursor = newCursor
                      }
                    , Buffer.insert state.cursor insertString buffer
                    , Cmd.batch [ debounceCmd, scrollCmd ]
                    )
                        |> recordHistory state buffer

        FirstLine ->
            let
                cursor =
                    { line = 0, column = 0 }
            in
            ( { state | cursor = cursor, selection = Nothing }, buffer, setEditorViewportForLine cursor.line ) |> recordHistory state buffer

        AcceptLineNumber nString ->
            case String.toInt nString of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just n_ ->
                    let
                        lineNumber =
                            clamp 0 (List.length (Buffer.lines buffer) - 1) (n_ - 1)

                        cursor =
                            { line = lineNumber, column = 0 }
                    in
                    ( { state | cursor = cursor, selection = Nothing }, buffer, setEditorViewportForLine lineNumber ) |> recordHistory state buffer

        AcceptSearchText str ->
            scrollToTextInternal str state buffer

        ScrollToSelection ( start, end ) ->
            ( state, buffer, Cmd.none )

        RollSearchSelectionForward ->
            rollSearchSelectionForward state buffer

        RollSearchSelectionBackward ->
            rollSearchSelectionBackward state buffer

        AcceptReplacementText str ->
            ( { state | replacementText = str }, buffer, Cmd.none )

        ReplaceCurrentSelection ->
            case state.selection of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just end ->
                    let
                        newBuffer =
                            Buffer.replace state.cursor end state.replacementText buffer
                    in
                    rollSearchSelectionForward state newBuffer
                        |> recordHistory state buffer

        LastLine ->
            let
                cursor =
                    { line = List.length (Buffer.lines buffer) - 1, column = 0 }
            in
            ( { state | cursor = cursor, selection = Nothing }, buffer, setEditorViewportForLine cursor.line ) |> recordHistory state buffer

        RemoveCharAfter ->
            case state.selection of
                Just selection ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                    ( { state
                        | cursor = start
                        , selection = Nothing
                      }
                    , Buffer.replace start end "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    ( state
                    , Buffer.replace
                        state.cursor
                        (Position.nextColumn state.cursor)
                        ""
                        buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        RemoveCharBefore ->
            case state.selection of
                Just selection ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                    ( { state
                        | cursor = start
                        , selection = Nothing
                      }
                    , Buffer.replace start end "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    ( { state
                        | cursor =
                            Position.previousColumn state.cursor
                                -- use old buffer to place cursor at the
                                -- end of the old line
                                |> Buffer.clampPosition Buffer.Backward buffer
                      }
                    , Buffer.removeBefore state.cursor buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        RemoveGroupAfter ->
            case state.selection of
                Just selection ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                    ( { state
                        | cursor = start
                        , selection = Nothing
                      }
                    , Buffer.replace start end "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    let
                        end =
                            Buffer.groupEnd state.cursor buffer
                    in
                    ( state
                    , Buffer.replace state.cursor end "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        Copy ->
            case state.selection of
                Nothing ->
                    ( { state | selectedText = Nothing }, buffer, Cmd.none ) |> recordHistory state buffer

                Just sel ->
                    (let
                        ( start, end ) =
                            Position.order sel state.cursor

                        selectedText =
                            Buffer.between start end buffer
                     in
                     ( { state | selectedText = Just selectedText }, buffer, Cmd.none )
                    )
                        |> recordHistory state buffer

        Cut ->
            case state.selection of
                Nothing ->
                    ( { state | selectedText = Nothing }, buffer, Cmd.none ) |> recordHistory state buffer

                Just sel ->
                    (let
                        ( start, end ) =
                            Position.order sel state.cursor

                        selectedText =
                            Buffer.between start end buffer
                     in
                     ( { state | selectedText = Just selectedText, selection = Nothing }, Buffer.replace start end "" buffer, Cmd.none )
                    )
                        |> recordHistory state buffer

        RemoveGroupBefore ->
            case state.selection of
                Just selection ->
                    let
                        ( start, end ) =
                            Position.order selection state.cursor
                    in
                    ( { state
                        | cursor = start
                        , selection = Nothing
                      }
                    , Buffer.replace start end "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    let
                        start =
                            Buffer.groupStart state.cursor buffer
                    in
                    ( { state | cursor = start }
                    , Buffer.replace start state.cursor "" buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        Indent ->
            case state.selection of
                Just selection ->
                    ( { state
                        | cursor =
                            Position.addColumn
                                Buffer.indentSize
                                state.cursor
                        , selection =
                            Just <|
                                Position.addColumn
                                    Buffer.indentSize
                                    selection
                      }
                    , Buffer.indentBetween state.cursor selection buffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    let
                        ( indentedBuffer, indentedColumn ) =
                            Buffer.indentFrom state.cursor buffer
                    in
                    ( { state
                        | cursor =
                            Position.setColumn indentedColumn state.cursor
                      }
                    , indentedBuffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        Deindent ->
            case state.selection of
                Just selection ->
                    let
                        ( deindentedBuffer, cursorColumn, selectionColumn ) =
                            Buffer.deindentBetween state.cursor selection buffer
                    in
                    ( { state
                        | cursor =
                            Position.setColumn cursorColumn state.cursor
                        , selection =
                            Just <|
                                Position.setColumn selectionColumn selection
                      }
                    , deindentedBuffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

                Nothing ->
                    let
                        ( deindentedBuffer, deindentedColumn ) =
                            Buffer.deindentFrom state.cursor buffer
                    in
                    ( { state
                        | cursor =
                            Position.setColumn deindentedColumn state.cursor
                      }
                    , deindentedBuffer
                    , Cmd.none
                    )
                        |> recordHistory state buffer

        SelectUp ->
            let
                cursor =
                    Position.previousLine state.cursor
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectDown ->
            let
                cursor =
                    Position.nextLine state.cursor
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectLeft ->
            let
                cursor =
                    Position.previousColumn state.cursor
                        |> Buffer.clampPosition Buffer.Backward buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectRight ->
            let
                cursor =
                    Position.nextColumn state.cursor
                        |> Buffer.clampPosition Buffer.Forward buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectToLineStart ->
            let
                cursor =
                    Position.setColumn 0 state.cursor
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , Cmd.none
            )

        SelectToLineEnd ->
            let
                cursor =
                    Position.setColumn
                        (Buffer.lineEnd state.cursor.line buffer
                            |> Maybe.withDefault state.cursor.line
                        )
                        state.cursor
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , Cmd.none
            )

        SelectToGroupStart ->
            let
                cursor =
                    Buffer.groupStart state.cursor buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectToGroupEnd ->
            let
                cursor =
                    Buffer.groupEnd state.cursor buffer
            in
            ( { state
                | cursor = cursor
                , selection =
                    if state.selection == Just cursor then
                        Nothing

                    else if
                        (state.selection == Nothing)
                            && (state.cursor /= cursor)
                    then
                        Just state.cursor

                    else
                        state.selection
              }
            , buffer
            , setEditorViewportForLine cursor.line
            )

        SelectAll ->
            ( { state
                | cursor = Buffer.lastPosition buffer
                , selection = Just (Position 0 0)
              }
            , buffer
            , setEditorViewportForLine 0
            )

        SelectGroup ->
            let
                range =
                    Buffer.groupRange state.cursor buffer
            in
            case range of
                Just ( start, end ) ->
                    ( { state | cursor = end, selection = Just start }
                    , buffer
                    , Cmd.none
                    )

                Nothing ->
                    ( state, buffer, Cmd.none )

        SelectLine ->
            ( { state
                | cursor =
                    Buffer.lineEnd state.cursor.line buffer
                        |> Maybe.map
                            (\column ->
                                Position.setColumn column state.cursor
                            )
                        |> Maybe.withDefault state.cursor
                , selection = Just <| Position.setColumn 0 state.cursor
              }
            , buffer
            , Cmd.none
            )

        SendLine ->
            let
                k =
                    state.cursor.line - state.window.first

                newWindow =
                    Window.shift (k - 5) state.window
            in
            -- ( { state | currentLine = Buffer.lineAt state.cursor buffer, window = newWindow }, buffer, Cmd.none )
            ( { state | currentLine = Buffer.lineAt state.cursor buffer }, buffer, Cmd.none )

        Undo ->
            case Editor.History.undo (stateToSnapshot state buffer) state.history of
                Just ( history, snapshot ) ->
                    ( { state
                        | cursor = snapshot.cursor
                        , selection = snapshot.selection
                        , history = history
                      }
                    , snapshot.buffer
                    , setEditorViewportForLine snapshot.cursor.line
                    )

                Nothing ->
                    ( state, buffer, Cmd.none )

        Redo ->
            case Editor.History.redo (stateToSnapshot state buffer) state.history of
                Just ( history, snapshot ) ->
                    ( { state
                        | cursor = snapshot.cursor
                        , selection = snapshot.selection
                        , history = history
                      }
                    , snapshot.buffer
                    , setEditorViewportForLine snapshot.cursor.line
                    )

                Nothing ->
                    ( state, buffer, Cmd.none )

        ScrollUp k ->
            let
                newCursor =
                    Position.shift -k state.cursor
            in
            ( { state | cursor = newCursor, selection = Nothing }, buffer, setEditorViewportForLine newCursor.line )

        ScrollDown k ->
            let
                newCursor =
                    Position.shift k state.cursor
            in
            ( { state | cursor = newCursor, selection = Nothing }, buffer, setEditorViewportForLine newCursor.line )

        Clear ->
            ( clearState state, Buffer.init "", Cmd.none )

        WrapAll ->
            ( state, Buffer.init (Editor.Wrap.paragraphs state.config (Buffer.toString buffer)), Cmd.none )
                |> recordHistory state buffer

        WrapSelection ->
            case state.selection of
                Nothing ->
                    ( state, buffer, Cmd.none )

                Just sel ->
                    let
                        ( start, end ) =
                            Position.order sel state.cursor

                        selectedText =
                            Buffer.between start end buffer

                        wrappedText =
                            Editor.Wrap.paragraphs state.config selectedText

                        newState =
                            { state | selectedText = Just selectedText }

                        newBuffer =
                            Buffer.replace start end wrappedText buffer
                    in
                    ( newState, newBuffer, Cmd.none )
                        |> recordHistory state buffer

        ToggleWrapping ->
            if state.config.wrapOption == DoWrap then
                ( setWrapOption DontWrap state, buffer, Cmd.none )

            else
                ( setWrapOption DoWrap state, buffer, Cmd.none )

        ToggleHelp ->
            if state.showHelp == True then
                let
                    ( newState, newBuffer ) =
                        load state.config.wrapOption Editor.Strings.help state
                in
                ( { newState | showHelp = False, savedBuffer = buffer }, newBuffer, Cmd.none )

            else
                ( { state | showHelp = True, savedBuffer = Buffer.fromString "" }
                , state.savedBuffer
                , Cmd.none
                )

        ToggleInfoPanel ->
            ( { state | showInfoPanel = not state.showInfoPanel }, buffer, Cmd.none )

        ToggleGoToLinePanel ->
            if state.showGoToLinePanel == True then
                ( { state | showGoToLinePanel = False }, buffer, blur "line-number-input" )

            else
                ( { state | showGoToLinePanel = True }, buffer, focus "line-number-input" )

        ToggleSearchPanel ->
            if state.showSearchPanel == True then
                ( { state | showSearchPanel = False }, buffer, blur "search-box" )

            else
                ( { state | showSearchPanel = True }, buffer, focus "search-box" )

        ToggleReplacePanel ->
            if state.showSearchPanel == True then
                ( { state | showSearchPanel = False, canReplace = False }, buffer, blur "search-box" )

            else
                ( { state | showSearchPanel = True, canReplace = True }, buffer, focus "search-box" )

        OpenReplaceField ->
            ( { state | canReplace = True }, buffer, Cmd.none )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast unload)
                        msg_
                        state.debounce
            in
            ( { state | debounce = debounce }, buffer, cmd )

        Unload str ->
            ( { state | debounce = state.debounce }, buffer, Cmd.none )



-- HELPERS --


setViewportForElement : (Result Dom.Error ( Dom.Element, Dom.Viewport ) -> msg) -> String -> String -> Cmd msg
setViewportForElement msg vpId id =
    Dom.getViewportOf vpId
        |> Task.andThen (\vp -> getElementWithViewPort vp id)
        |> Task.attempt msg


getElementWithViewPort : Dom.Viewport -> String -> Task Dom.Error ( Dom.Element, Dom.Viewport )
getElementWithViewPort vp id =
    Dom.getElement id
        |> Task.map (\el -> ( el, vp ))


setEditorViewportForLine : Int -> Cmd Msg
setEditorViewportForLine lineNumber =
    let
        y =
            toFloat lineNumber * 16.0

        -- viewport.viewport.y + element.element.y - element.element.height - 100
    in
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf "__inner_editor__" 0 y)


unload : String -> Cmd Msg
unload s =
    Task.perform Unload (Task.succeed s)


{-| Return a pair (InternalState, Buffer) representing the editor scrolled
to a given line `k`.
-}
scrollToLine : Int -> InternalState -> Buffer -> ( InternalState, Buffer )
scrollToLine k state buffer =
    let
        n =
            clamp 0 (List.length (Buffer.lines buffer) - 1) (k - 1)

        cursor =
            { line = n, column = 0 }

        window =
            Window.scrollToIncludeCursor cursor state.window
    in
    ( { state | cursor = cursor, window = window, selection = Nothing }, buffer )



{-

   TODO: Currently search on a string returns hits which are words.  They should be exact matches.

-}


{-| Load string into editor with option to wrap it.
-}
load : WrapOption -> String -> InternalState -> ( InternalState, Buffer )
load wrapOption content state =
    let
        config =
            state.config

        lineLengths =
            String.lines content |> List.map String.length

        maxLineLength =
            List.maximum lineLengths |> Maybe.withDefault 1000

        buffer =
            if wrapOption == DoWrap then
                Buffer.fromString (Editor.Wrap.paragraphs config content)

            else
                Buffer.fromString content
    in
    ( clearState state, buffer )


{-| Search for str and scroll to first hit. Used internally.
-}
scrollToTextInternal : String -> InternalState -> Buffer -> ( InternalState, Buffer, Cmd Msg )
scrollToTextInternal str state buffer =
    let
        searchResults =
            Editor.Search.search str buffer
    in
    case List.head searchResults of
        Nothing ->
            ( { state | searchResults = RollingList.fromList [], searchTerm = str, selection = Nothing }, buffer, Cmd.none )

        Just ( cursor, end ) ->
            let
                window_ =
                    Window.scrollToIncludeCursor cursor state.window

                ( cursor_, end_ ) =
                    ( Window.shiftPosition__ window_ cursor, Window.shiftPosition__ window_ end )
            in
            ( { state | window = window_, cursor = cursor_, selection = Just end_, searchResults = RollingList.fromList searchResults, searchTerm = str }, buffer, Cmd.none )


{-| Return data representing the editor scrolled toa given string (first search occurrence).
-}
scrollToText : String -> InternalState -> Buffer -> ( InternalState, Buffer )
scrollToText str state buffer =
    let
        searchResults =
            Editor.Search.search str buffer
    in
    case List.head searchResults of
        Nothing ->
            ( { state | searchResults = RollingList.fromList [], searchTerm = str }, buffer )

        Just ( cursor, end ) ->
            let
                window_ =
                    Window.scrollToIncludeCursor cursor state.window

                ( cursor_, end_ ) =
                    ( cursor, end )
            in
            ( { state | window = window_, cursor = cursor_, selection = Just end_, searchResults = RollingList.fromList searchResults, searchTerm = str }, buffer )


rollSearchSelectionForward : InternalState -> Buffer -> ( InternalState, Buffer, Cmd Msg )
rollSearchSelectionForward state buffer =
    let
        searchResults_ =
            RollingList.roll state.searchResults
    in
    case RollingList.current searchResults_ of
        Nothing ->
            ( state, buffer, Cmd.none )

        Just ( cursor, end ) ->
            let
                window_ =
                    Window.scrollToIncludeCursor cursor state.window

                ( cursor_, end_ ) =
                    ( Window.shiftPosition__ window_ cursor, Window.shiftPosition__ window_ end )
            in
            ( { state
                | cursor = cursor_
                , window = window_
                , selection = Just end_
                , searchResults = searchResults_
              }
            , buffer
            , Cmd.none
            )


rollSearchSelectionBackward : InternalState -> Buffer -> ( InternalState, Buffer, Cmd Msg )
rollSearchSelectionBackward state buffer =
    let
        searchResults_ =
            RollingList.rollBack state.searchResults
    in
    case RollingList.current searchResults_ of
        Nothing ->
            ( state, buffer, Cmd.none )

        Just ( cursor, end ) ->
            let
                window_ =
                    Window.scrollToIncludeCursor cursor state.window

                ( cursor_, end_ ) =
                    ( Window.shiftPosition__ window_ cursor, Window.shiftPosition__ window_ end )
            in
            ( { state
                | cursor = cursor_
                , window = window_
                , selection = Just end_
                , searchResults = searchResults_
              }
            , buffer
            , Cmd.none
            )


setMaximumWrapWidth : Int -> InternalState -> InternalState
setMaximumWrapWidth k state =
    lift (Config.setMaximumWrapWidth k) state


setOptimumWrapWidth : Int -> InternalState -> InternalState
setOptimumWrapWidth k state =
    lift (Config.setOptimumWrapWidth k) state


setWrapOption : WrapOption -> InternalState -> InternalState
setWrapOption wrapOption state =
    lift (Config.setWrapOption wrapOption) state


lift : (Config -> Config) -> (InternalState -> InternalState)
lift f =
    \is -> { is | config = f is.config }


{-| Return the "zero"" internal state
-}
clearState : InternalState -> InternalState
clearState state =
    { state
        | window = { first = 0, last = state.config.lines - 1 }
        , cursor = { line = 0, column = 0 }
        , selection = Nothing
        , selectedText = Nothing
        , dragging = False
        , searchTerm = ""
        , replacementText = ""
        , scrolledLine = 0
        , searchResults = RollingList.fromList []
    }


{-| A command to place the focus on the element with
the given id.
-}
focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


{-| A command to lose focus on the element with
the given id.
-}
blur : String -> Cmd Msg
blur id =
    Task.attempt (\_ -> NoOp) (Dom.blur id)
