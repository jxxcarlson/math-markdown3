-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Document_update_column exposing (Document_update_column(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| update columns of table "document"

  - AuthorIdentifier - column name
  - ChildInfo - column name
  - ChildLevels - column name
  - Children - column name
  - Content - column name
  - DocType - column name
  - Id - column name
  - Public - column name
  - Slug - column name
  - Tags - column name
  - TimeStamp - column name
  - Title - column name

-}
type Document_update_column
    = AuthorIdentifier
    | ChildInfo
    | ChildLevels
    | Children
    | Content
    | DocType
    | Id
    | Public
    | Slug
    | Tags
    | TimeStamp
    | Title


list : List Document_update_column
list =
    [ AuthorIdentifier, ChildInfo, ChildLevels, Children, Content, DocType, Id, Public, Slug, Tags, TimeStamp, Title ]


decoder : Decoder Document_update_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "authorIdentifier" ->
                        Decode.succeed AuthorIdentifier

                    "childInfo" ->
                        Decode.succeed ChildInfo

                    "childLevels" ->
                        Decode.succeed ChildLevels

                    "children" ->
                        Decode.succeed Children

                    "content" ->
                        Decode.succeed Content

                    "docType" ->
                        Decode.succeed DocType

                    "id" ->
                        Decode.succeed Id

                    "public" ->
                        Decode.succeed Public

                    "slug" ->
                        Decode.succeed Slug

                    "tags" ->
                        Decode.succeed Tags

                    "timeStamp" ->
                        Decode.succeed TimeStamp

                    "title" ->
                        Decode.succeed Title

                    _ ->
                        Decode.fail ("Invalid Document_update_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Document_update_column -> String
toString enum =
    case enum of
        AuthorIdentifier ->
            "authorIdentifier"

        ChildInfo ->
            "childInfo"

        ChildLevels ->
            "childLevels"

        Children ->
            "children"

        Content ->
            "content"

        DocType ->
            "docType"

        Id ->
            "id"

        Public ->
            "public"

        Slug ->
            "slug"

        Tags ->
            "tags"

        TimeStamp ->
            "timeStamp"

        Title ->
            "title"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Document_update_column
fromString enumString =
    case enumString of
        "authorIdentifier" ->
            Just AuthorIdentifier

        "childInfo" ->
            Just ChildInfo

        "childLevels" ->
            Just ChildLevels

        "children" ->
            Just Children

        "content" ->
            Just Content

        "docType" ->
            Just DocType

        "id" ->
            Just Id

        "public" ->
            Just Public

        "slug" ->
            Just Slug

        "tags" ->
            Just Tags

        "timeStamp" ->
            Just TimeStamp

        "title" ->
            Just Title

        _ ->
            Nothing
