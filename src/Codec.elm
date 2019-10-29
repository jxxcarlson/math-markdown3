module Codec exposing (getPair, getPermission)

import Document exposing (Permission(..), UserPermission(..))
import Maybe.Extra
import Parser exposing (..)
import Prng.Uuid as Uuid exposing (Uuid)


type alias MaybePair =
    { u : Maybe Uuid
    , k : Int
    }


type alias Pair =
    ( Uuid, Int )


getPermission : String -> Maybe UserPermission
getPermission str_ =
    run permission str_
        |> Result.toMaybe


permission : Parser UserPermission
permission =
    succeed UserPermission
        |. symbol "("
        |. spaces
        |= (str |> map validUsername)
        |. spaces
        |. symbol ","
        |. spaces
        |= (str |> map permissionOfString)
        |. spaces
        |. symbol ")"


validUsername : String -> String
validUsername str_ =
    case String.length str_ < 2 of
        True ->
            "__noe_ueer__"

        False ->
            str_


permissionOfString : String -> Permission
permissionOfString str_ =
    case str_ of
        "ReadPermission" ->
            ReadPermission

        "WritePermission" ->
            WritePermission

        "NoPermission" ->
            NoPermission

        _ ->
            NoPermission


getPair : String -> Maybe Pair
getPair str_ =
    run pair str_
        |> Result.toMaybe
        |> Maybe.Extra.join


pair : Parser (Maybe Pair)
pair =
    (succeed MaybePair
        |. symbol "("
        |. spaces
        |= (str |> map Uuid.fromString)
        |. spaces
        |. symbol ","
        |. spaces
        |= int
        |. spaces
        |. symbol ")"
    )
        |> map fixup



--     |> map fixup


fixup : MaybePair -> Maybe Pair
fixup mPair =
    case ( mPair.u, mPair.k ) of
        ( Just uuid, k ) ->
            Just ( uuid, k )

        _ ->
            Nothing


str : Parser String
str =
    getChompedString <|
        succeed identity
            |. parseWhile (\c -> c /= ',')


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString
