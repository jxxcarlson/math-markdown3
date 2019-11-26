module Yaml exposing (fromDocument, fromDocumentList, required, toDocument, toDocumentList)

import Document exposing (DocType(..), Document, Permission(..), UserPermission(..))
import Parser exposing ((|.), (|=), Parser)
import Prng.Uuid as Uuid exposing (Uuid)
import Yaml.Decode as Decode exposing (Decoder)


{-| Output validated at <https://codebeautify.org/yaml-validator>
-}
fromDocumentList : List Document -> String
fromDocumentList noteList =
    "---\n"
        ++ List.foldl (\document acc -> fromDocument document ++ acc) "" noteList


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


toDocumentList : String -> Maybe (List Document)
toDocumentList str =
    Decode.fromString documentListDecoder str
        |> Result.toMaybe


toDocument : String -> Maybe Document
toDocument str =
    Decode.fromString documentDecoder str
        |> Result.toMaybe



-- DECODERS --


documentListDecoder : Decoder (List Document)
documentListDecoder =
    Decode.list documentDecoder


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


decodeChildInfoItem : Decoder ( Uuid, Int )
decodeChildInfoItem =
    Decode.string
        |> Decode.andThen decoderChildInfoItemAux


decoderChildInfoItemAux : String -> Decoder ( Uuid, Int )
decoderChildInfoItemAux str =
    Decode.string
        |> Decode.map (Parser.run childInfoItemAuxParser)
        |> Decode.andThen handleItemParseResult


handleItemParseResult : Result (List Parser.DeadEnd) ( Uuid, Int ) -> Decoder ( Uuid, Int )
handleItemParseResult result =
    case result of
        Ok pair ->
            Decode.succeed pair

        Err _ ->
            Decode.fail "Error parsing permission"


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



-- PARSERS --


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



-- PIPELINE


custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom =
    Decode.map2 (|>)


{-|

    import Yaml.Decode as Decode exposing (Decoder)

    type alias Foo =
        { bar : String
        , baz : Int
        }

    fooDecoder : Decoder Foo
    fooDecoder =
       Decode.succeed Foo
            |> required "bar" Decode.string
            |> required "baz" Decode.int

    str : String
    str =
        """   bar: hohoho!
       baz: 43
    """

    Decode.fromString fooDecoder str
    --> Ok { bar = "hohoho!", baz = 43 }

-}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
    custom (Decode.field key valDecoder) decoder


stringFromBool : Bool -> String
stringFromBool bit =
    case bit of
        True ->
            "True"

        False ->
            "False"



-- HELPERS


boolFromString : String -> Bool
boolFromString str =
    if str == "True" then
        True

    else
        False
