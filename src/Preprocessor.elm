module Preprocessor exposing (apply)


apply : String -> String
apply str =
    str
        |> String.lines
        |> List.map processLine
        |> String.join "\n"


processLine : String -> String
processLine str =
    if str == "|dm" then
        "$$\nxxx\n$$"

    else if str == "|im" then
        "$ xxx $"

    else
        str
