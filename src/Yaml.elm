module Yaml exposing (fromDocument, fromDocumentList)

import Document exposing (DocType(..), Document, Permission(..), UserPermission(..))
import Parser exposing ((|.), (|=), Parser)
import Prng.Uuid as Uuid exposing (Uuid)
import Utility
import Yaml.Decode as Decode exposing (Decoder)


{-| Output validated at <https://codebeautify.org/yaml-validator>
-}
fromDocumentList : List Document -> String
fromDocumentList noteList =
    "---\n"
        ++ List.foldl (\document acc -> fromDocument document ++ acc) "" noteList


toDocument : String -> Document
toDocument str =
    let
        doc =
            Document.dummy
    in
    doc


type alias Document =
    { id : Uuid
    , title : String
    , authorIdentifier : String
    , content : String
    , public : Bool
    , tags : List String
    , slug : String
    , docType : DocType
    , childInfo : List ( Uuid, Int )
    , permissions : List UserPermission
    }


documentDecoder : Decoder Document
documentDecoder =
    Decode.succeed Document
        |> required "id" decoderId
        |> required "title" Decode.string
        |> required "authorIdentifier" Decode.string
        |> required "content" Decode.string
        |> required "public" (Decode.string |> Decode.map boolFromString)
        |> required "tags" (Decode.list Decode.string)
        |> required "slug" Decode.string
        |> required "docType" (Decode.string |> Decode.map Document.docTypeFromString)
        |> required "childInfo" (Decode.list decodeChildInfoItem)
        |> required "permissions" (Decode.list decodePermission)


decodePermission : Decoder UserPermission
decodePermission =
    Decode.string |> Decode.andThen decodePermissionAux


decodePermissionAux : String -> Decoder UserPermission
decodePermissionAux str =
    case getPermission str of
        Just up ->
            Decode.succeed up

        Nothing ->
            Decode.fail "Bad string for user permissin"


getPermission : String -> Maybe UserPermission
getPermission str =
    Parser.run parseUserPermission str
        |> Result.toMaybe


parseUserPermission : Parser UserPermission
parseUserPermission =
    Parser.succeed (\username p -> UserPermission username p)
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseStringToChar ','
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= parsePermission
        |. Parser.spaces
        |. Parser.symbol ")"


parsePermission : Parser Document.Permission
parsePermission =
    parseStringToChar ')'
        |> Parser.map Document.permissionFromString


decodeChildInfoItem : Decoder ( Uuid, Int )
decodeChildInfoItem =
    Decode.string
        |> Decode.andThen decoderChildInfoItemAux


decoderChildInfoItemAux : String -> Decoder ( Uuid, Int )
decoderChildInfoItemAux str =
    Decode.string
        |> Decode.map (Parser.run childInfoItemAuxParser)
        |> unwrap


unwrap : Decoder (Result (List Parser.DeadEnd) ( Uuid, Int )) -> Decoder ( Uuid, Int )
unwrap =
    Debug.todo "dd"



--(Decode.Decoder result) =
--  Ok data -> Decode.succeed data
--  Err _ -> Decode.fail "Invalid child info data"


type alias ChildInfoItem =
    ( Uuid, Int )


childInfoItemAuxParser : Parser ( Uuid, Int )
childInfoItemAuxParser =
    Parser.succeed (\uuid k -> ( uuid, k ))
        |. Parser.symbol "("
        |. Parser.spaces
        |= uuidParser
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ")"


uuidParser : Parser Uuid
uuidParser =
    parseStringToChar ',' |> Parser.andThen uuidParserAux


uuidParserAux : String -> Parser Uuid
uuidParserAux str =
    case Uuid.fromString str of
        Just uuid ->
            Parser.succeed uuid

        Nothing ->
            Parser.problem "Bad uuid string"


parseStringToChar : Char -> Parser String
parseStringToChar endChar =
    (Parser.getChompedString <|
        Parser.succeed identity
            |. parseWhile (\c -> c /= endChar)
    )
        |> Parser.map String.trim


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    Parser.chompWhile accepting |> Parser.getChompedString


decoderId : Decoder Uuid
decoderId =
    Decode.field "id" Decode.string
        |> Decode.andThen decoderIdAux


decoderIdAux : String -> Decoder Uuid
decoderIdAux str =
    case Uuid.fromString str of
        Just uuid ->
            Decode.succeed uuid

        Nothing ->
            Decode.fail "invalid id"


custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom =
    Decode.map2 (|>)


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
    custom (Decode.field key valDecoder) decoder



-- (Yaml.Decode.string |> Yaml.Decode.map (Uuid.fromString |> Maybe.withDefault Utility.id0))
--        |> Yaml.Decode.andThen (Yaml.Decode.string "title")
--        |> Yaml.Decode.andThen (Yaml.Decode.string "authorIdentifier")
--        |> Yaml.Decode.andThen (Yaml.Decode.string "content")
--        |> Yaml.Decode.andThen (Yaml.Decode.string "public" |> Yaml.Decode.map boolFromString)
--        |> Yaml.Decode.andThen (Yaml.Decode.list "tags" |> Yaml.Decode.map boolFromString)


fromDocument : Document -> String
fromDocument document =
    "- document\n"
        ++ "   - id: "
        ++ Uuid.toString document.id
        ++ "\n"
        ++ "   - authorIdentifier: "
        ++ document.authorIdentifier
        ++ "\n"
        ++ "   - content"
        ++ document.content
        ++ "\n"
        ++ "   - public"
        ++ (document.public |> stringFromBool)
        ++ "\n"
        ++ "   - tags: ["
        ++ String.join ", " document.tags
        ++ "]\n"
        ++ "   - slug"
        ++ document.content
        ++ "\n"
        ++ "   - docType: "
        ++ (document.docType |> Document.stringFromDocType)
        ++ "\n"
        ++ "   - timeModified: "
        ++ (document.childInfo |> Document.stringFromChildInfo)
        ++ "\n"
        ++ "   - permissions: "
        ++ (document.permissions |> Document.stringFromPermissions)
        ++ "\n"


stringFromBool : Bool -> String
stringFromBool bit =
    case bit of
        True ->
            "True"

        False ->
            "False"


boolFromString : String -> Bool
boolFromString str =
    if str == "True" then
        True

    else
        False
