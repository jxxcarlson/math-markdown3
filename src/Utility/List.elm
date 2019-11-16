module Utility.List exposing (insertIntegerAtIndex, insertItem, insertString, insertUuid)

import List.Extra
import Prng.Uuid exposing (Uuid(..))


insertItem : (a -> a -> Bool) -> a -> a -> List a -> List a
insertItem equal newItem targetItem list =
    case List.Extra.splitWhen (\element -> equal element targetItem) list of
        Just ( a, b ) ->
            case List.head b of
                Nothing ->
                    a ++ (newItem :: b)

                Just x ->
                    a ++ (x :: newItem :: List.drop 1 b)

        Nothing ->
            list


{-|

    > insertString "x" "b" ["a", "b", "c"]
    ---> ["a","b","x","c"]

-}
insertString : String -> String -> List String -> List String
insertString newString targetString list =
    case list == [] of
        True ->
            [ newString ]

        False ->
            case List.Extra.splitWhen (\element -> element == targetString) list of
                Just ( a, b ) ->
                    case List.head b of
                        Nothing ->
                            a ++ (newString :: b)

                        Just x ->
                            a ++ (x :: newString :: List.drop 1 b)

                Nothing ->
                    list


insertUuid : Uuid -> Uuid -> List Uuid -> List Uuid
insertUuid newUuid targetUuid list =
    case list == [] of
        True ->
            [ newUuid ]

        False ->
            case List.Extra.splitWhen (\element -> element == targetUuid) list of
                Just ( a, b ) ->
                    case List.head b of
                        Nothing ->
                            a ++ (newUuid :: b)

                        Just x ->
                            a ++ (x :: newUuid :: List.drop 1 b)

                Nothing ->
                    list


{-|

> insertIntegerAtIndex 7 1 [0, 0, 0][0,0,7,0] : List Int

-}
insertIntegerAtIndex : Int -> Int -> List Int -> List Int
insertIntegerAtIndex newInt idx list =
    case list == [] of
        True ->
            [ newInt ]

        False ->
            let
                ( a, b ) =
                    List.Extra.splitAt idx list
            in
            case List.head b of
                Nothing ->
                    a ++ (newInt :: b)

                Just x ->
                    a ++ (x :: newInt :: List.drop 1 b)
