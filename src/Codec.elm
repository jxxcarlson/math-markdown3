module Codec exposing (getPair, getUserPermission, userPermission)

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


getUserPermission : String -> Maybe UserPermission
getUserPermission str_ =
    run userPermission str_
        |> Result.toMaybe


userPermission : Parser UserPermission
userPermission =
    succeed UserPermission
        |. symbol "{"
        |. spaces
        |= (strForJson |> map validUsername)
        |. spaces
        |. symbol ":"
        |. spaces
        |= (strForJson |> map permissionOfString)
        |. spaces
        |. symbol "}"


validUsername : String -> String
validUsername str_ =
    case String.length str_ < 2 of
        True ->
            "__no_user__"

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
        |= (strForPair |> map Uuid.fromString)
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


strForJson : Parser String
strForJson =
    getChompedString <|
        succeed identity
            |. parseWhile (\c -> c /= ':' && c /= '}')


strForPair : Parser String
strForPair =
    getChompedString <|
        succeed identity
            |. parseWhile (\c -> c /= ',')


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString
