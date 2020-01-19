module Sync exposing (getId)

import Dict exposing (Dict)
import Markdown.Option exposing (..)
import Markdown.Parse as Parse


{-| Return values whose keys contain the given string
-}
getId1 : String -> Dict String String -> Maybe String
getId1 str_ sourceMap =
    let
        str =
            Parse.toMDBlockTree 66 ExtendedMath str_ |> Parse.getLeadingTextFromAST |> String.trim
    in
    Dict.get str sourceMap


{-| Return values whose keys contain the given string
-}
getId : String -> Dict String String -> Maybe String
getId str_ sourceMap =
    let
        str =
            Parse.toMDBlockTree 66 ExtendedMath str_ |> Parse.getLeadingTextFromAST |> String.trim
    in
    List.filter (\( k, v ) -> String.contains str k) (Dict.toList sourceMap)
        |> List.map (\( source, id ) -> id)
        |> List.head
