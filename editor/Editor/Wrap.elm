module Editor.Wrap exposing (paragraphs, runFSM)

{-| Code for wrapping text. This needs more thought/work.
-}

import Dict exposing (Dict)
import Editor.Config exposing (Config)
import Paragraph


{-| Wrap text preserving paragraph structure and code blocks
-}
paragraphs : Config -> String -> String
paragraphs config str =
    str
        |> runFSM
        |> List.map (wrapParagraph config)
        |> String.join "\n\n"


{-| Wrap text in paragraph if it is of ParagraphType,
but not if it is of codeType
-}
wrapParagraph : Config -> ( ParagraphType, String ) -> String
wrapParagraph config ( paragraphType, str ) =
    case paragraphType of
        TextParagraph ->
            Paragraph.lines config.wrapParams str |> String.join "\n"

        CodeParagraph ->
            str


{-| Run a finite-state machine that gathers logical paragraphs
into a list of tuples ( ParagraphType, String ) wheree the
first component classifies the type of paragraph (as ParagraphType
or CodeType
-}
runFSM : String -> List ( ParagraphType, String )
runFSM str =
    let
        lines =
            String.lines str

        initialData =
            ( Start, { currentParagraph = [], paragraphList = [], tick = 0 } )
    in
    List.foldl nextState initialData lines
        |> Tuple.second
        |> .paragraphList
        |> List.reverse


{-| Then next-state function for the finite-state machine.
-}
nextState : String -> ( State, Data ) -> ( State, Data )
nextState line ( state, data ) =
    let
        ( newState, action ) =
            nextStateAndAction line state

        --        _ =
        --            Debug.log "line" ( state, line, data.currentParagraph )
    in
    ( newState, action line data )


{-| A dictionary of functions which carry out the actions
of the finite-state machine.
-}
opDict : Dict String (String -> Data -> Data)
opDict =
    Dict.fromList
        [ ( "NoOp", \s d -> d )
        , ( "AddToParagraph", \s d -> { d | currentParagraph = s :: d.currentParagraph, tick = d.tick + 1 } )
        , ( "AddToCode", \s d -> { d | currentParagraph = s :: d.currentParagraph, tick = d.tick + 1 } )
        , ( "EndCode", \s d -> { d | currentParagraph = [], paragraphList = ( CodeParagraph, joinLinesForCode <| s :: d.currentParagraph ) :: d.paragraphList } )
        , ( "EndParagraph", \s d -> { d | currentParagraph = [], paragraphList = ( TextParagraph, joinLines d.currentParagraph ) :: d.paragraphList } )
        , ( "StartCode", \s d -> { d | currentParagraph = [ s ], tick = d.tick + 1 } )
        , ( "StartCodeFromParagraph", \s d -> { d | currentParagraph = [ s ], paragraphList = ( TextParagraph, joinLines d.currentParagraph ) :: d.paragraphList, tick = d.tick + 1 } )
        , ( "StartCodeFromBlank", \s d -> { d | currentParagraph = [ s ], paragraphList = ( TextParagraph, joinLines d.currentParagraph ) :: d.paragraphList, tick = d.tick + 1 } )
        , ( "StartParagraph", \s d -> { d | currentParagraph = [ s ], tick = d.tick + 1 } )
        ]


{-| Look up the FSM function given its name.
-}
op : String -> (String -> Data -> Data)
op opName =
    Dict.get opName opDict |> Maybe.withDefault (\_ d -> d)


{-| Join the elements of a string lists with spaces.
-}
joinLines : List String -> String
joinLines list =
    list
        |> List.reverse
        |> List.filter (\s -> s /= "")
        |> String.join " "


{-| Join the elements of a string lists with newlines.
-}
joinLinesForCode : List String -> String
joinLinesForCode list =
    list
        |> List.reverse
        |> String.join "\n"


{-| Define the Finite State Machine
-}
nextStateAndAction : String -> State -> ( State, String -> Data -> Data )
nextStateAndAction line state =
    case ( state, classifyLine line ) of
        ( InParagraph, Text ) ->
            ( InParagraph, op "AddToParagraph" )

        ( InParagraph, Blank ) ->
            ( InBlank, op "EndParagraph" )

        ( InParagraph, CodeDelimiter ) ->
            ( InCode, op "StartCodeFromParagraph" )

        ( InBlank, Blank ) ->
            ( InBlank, op "EndParagraph" )

        ( InBlank, Text ) ->
            ( InParagraph, op "StartParagraph" )

        ( InBlank, CodeDelimiter ) ->
            ( InCode, op "StartCodeFromBlank" )

        ( Start, Text ) ->
            ( InParagraph, op "StartParagraph" )

        ( Start, CodeDelimiter ) ->
            ( InCode, op "StartCode" )

        ( Start, Blank ) ->
            ( Start, op "NoOp" )

        ( InCode, CodeDelimiter ) ->
            ( Start, op "EndCode" )

        ( InCode, Blank ) ->
            ( InCode, op "AddToCode" )

        ( InCode, Text ) ->
            ( InCode, op "AddToCode" )


type State
    = Start
    | InParagraph
    | InBlank
    | InCode


type LineType
    = Blank
    | Text
    | CodeDelimiter


type ParagraphType
    = TextParagraph
    | CodeParagraph


{-| Classify a line as Blank | CodeDelimiter or Text
-}
classifyLine : String -> LineType
classifyLine str =
    let
        prefix =
            String.trimLeft str
    in
    if prefix == "" then
        Blank

    else if String.left 3 prefix == "```" then
        CodeDelimiter

    else
        Text


{-| The data structure on which the finite-state machine operates.
-}
type alias Data =
    { currentParagraph : List String
    , paragraphList : List ( ParagraphType, String )
    , tick : Int
    }
