module Utility.String exposing (compress, ensureNotEmpty, normalize)


normalize : String -> String
normalize str =
    str
        |> String.words
        |> List.map (String.toLower >> silencePunctuation)
        |> filterEmpties
        |> String.join "-"


compress : String -> String
compress str =
    str
        |> String.words
        |> List.map String.toLower
        |> filterNoise
        |> List.map silencePunctuation
        -- |> List.indexedMap shorten
        -- |> fixup
        --        |> List.Extra.groupsOf 2
        --        |> List.indexedMap join
        |> List.filter (\item -> item /= "")
        |> String.join "-"


filterEmpties : List String -> List String
filterEmpties strListr =
    List.filter (\x -> x /= "") strListr


silencePunctuation : String -> String
silencePunctuation str =
    str
        |> String.split ""
        |> List.filter isAlphaNumOrWhiteSpace
        |> String.join ""


filterNoise : List String -> List String
filterNoise list =
    List.filter (\word -> not (List.member word lowInfoWords)) list


lowInfoWords =
    [ "a", "about", "the", "in", "is", "it", "its", "on", "of", "for", "to", "from", "with", "without", "that", "this", "and", "or" ]


isAlphaNumOrWhiteSpace : String -> Bool
isAlphaNumOrWhiteSpace x =
    List.member x [ " ", "\n", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "z", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]


ensureNotEmpty : String -> String -> String
ensureNotEmpty default str =
    case str of
        "" ->
            default

        _ ->
            str
