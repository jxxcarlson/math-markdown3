module AppNavigation exposing (NavigationType(..), classify, idSegment)

import Url
import Url.Parser exposing (..)


type NavigationType
    = TocRef
    | DocRef
    | IdRef
    | SubdocIdRef


classify : String -> ( NavigationType, String )
classify str =
    if String.left 4 str == "doc/" then
        ( DocRef, String.dropLeft 4 str )

    else if String.left 3 str == "id/" then
        ( IdRef, String.dropLeft 3 str )

    else if String.left 10 str == "subdoc-id/" then
        ( SubdocIdRef, String.dropLeft 10 str )

    else
        ( TocRef, str )


idSegment : Parser (String -> a) a
idSegment =
    s "#id" </> string
