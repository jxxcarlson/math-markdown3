module EditorTools exposing (findStringInAST, lineNumber, selectionDecoder)

import Json.Decode as D exposing (Decoder)
import Markdown.Parse as Parse
import Render exposing (RenderingData(..))


selectionDecoder : Decoder String
selectionDecoder =
    D.field "selection" D.string


lineNumber : String -> String -> Maybe Int
lineNumber key text =
    let
        key_ =
            String.toLower key
    in
    text
        |> String.toLower
        |> String.split "\n"
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, str ) -> String.contains key_ str)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.map (\x -> x + 1)


findStringInAST : String -> RenderingData msg -> Maybe ( Int, Int )
findStringInAST str rd =
    case rd of
        MD data ->
            Parse.searchAST str data.fullAst

        ML data ->
            Nothing
