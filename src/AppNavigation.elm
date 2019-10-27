module AppNavigation exposing (NavigationType(..), classify)


type NavigationType
    = TocRef
    | DocRef
    | IdRef


classify : String -> ( NavigationType, String )
classify str =
    if String.left 4 str == "doc/" then
        ( DocRef, String.dropLeft 4 str )

    else if String.left 3 str == "id/" then
        ( IdRef, String.dropLeft 3 str )

    else
        ( TocRef, str )
