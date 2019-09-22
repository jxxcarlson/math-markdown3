-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Document_constraint exposing (Document_constraint(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "document"

  - Documents\_identifier\_key - unique or primary key constraint
  - Documents\_pkey - unique or primary key constraint

-}
type Document_constraint
    = Documents_identifier_key
    | Documents_pkey


list : List Document_constraint
list =
    [ Documents_identifier_key, Documents_pkey ]


decoder : Decoder Document_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "documents_identifier_key" ->
                        Decode.succeed Documents_identifier_key

                    "documents_pkey" ->
                        Decode.succeed Documents_pkey

                    _ ->
                        Decode.fail ("Invalid Document_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Document_constraint -> String
toString enum =
    case enum of
        Documents_identifier_key ->
            "documents_identifier_key"

        Documents_pkey ->
            "documents_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Document_constraint
fromString enumString =
    case enumString of
        "documents_identifier_key" ->
            Just Documents_identifier_key

        "documents_pkey" ->
            Just Documents_pkey

        _ ->
            Nothing
