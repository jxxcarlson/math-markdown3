module Editor.Text exposing (prepare, prepareLines, prepareLinesWithWrapping, wrapLines)

{-| Code for wrapping text. This needs more thought/work.
-}

import Editor.Config exposing (Config)
import Paragraph


prepare : Config -> String -> String
prepare config str =
    str
        |> String.lines
        |> List.map (wrapParagraph config)
        |> String.join "\n\n"


prepareLines : Config -> String -> String
prepareLines config str =
    str
        |> String.split "\n\n"
        |> List.filter (\line -> line /= "\n")
        |> List.map (wrapParagraph config)
        |> String.join "\n\n"


prepareLinesWithWrapping : Config -> String -> String
prepareLinesWithWrapping config str =
    str
        |> String.lines
        |> wrapLines
        |> List.map (wrapParagraph config)
        |> String.join "\n\n"


wrapParagraph : Config -> String -> String
wrapParagraph config str =
    Paragraph.lines config.wrapParams str |> String.join "\n"


{-|

    wrapLines ["one","two","\n","three", "\n", "four", "five"]
    -> ["one two","three","four five"]

-}
wrapLines : List String -> List String
wrapLines strings =
    let
        start =
            ( BlankLine, { currentParagraph = "", paragraphList = [] } )

        ( state, data ) =
            List.foldl munch start strings
    in
    List.reverse <| data.currentParagraph :: data.paragraphList


munch : String -> ( State, Data ) -> ( State, Data )
munch line ( state, data ) =
    let
        nextState =
            if line == "\n" then
                BlankLine

            else
                InParagraph
    in
    case ( state, nextState ) of
        ( InParagraph, InParagraph ) ->
            ( nextState, { data | currentParagraph = data.currentParagraph ++ " " ++ line } )

        ( InParagraph, BlankLine ) ->
            ( nextState, { data | currentParagraph = "", paragraphList = data.currentParagraph :: data.paragraphList } )

        ( BlankLine, InParagraph ) ->
            ( nextState, { data | currentParagraph = line } )

        ( BlankLine, BlankLine ) ->
            ( nextState, data )


type State
    = InParagraph
    | BlankLine


type alias Data =
    { currentParagraph : String
    , paragraphList : List String
    }
