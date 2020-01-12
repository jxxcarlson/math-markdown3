module Utility.View exposing (hideIf, showIf, showOne)

import Element exposing (Element)
import Html.Attributes
import Model exposing (Msg)



-- VIEW UTILITIES: showIf, etc.


showIf : Bool -> Element Msg -> Element Msg
showIf bit element =
    if bit then
        element

    else
        Element.none


hideIf : Bool -> Element Msg -> Element Msg
hideIf bit element =
    if not bit then
        element

    else
        Element.none


showOne : Bool -> String -> String -> String
showOne bit str1 str2 =
    case bit of
        True ->
            str1

        False ->
            str2
