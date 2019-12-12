module Interchange exposing (documentDecoder, documentListDecoder, encodeDocument, encodeDocumentList)

import Document exposing (DocType(..), Document, UserPermission(..), dummy)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Prng.Uuid as Uuid exposing (Uuid)
import Utility



-- ENCODER


{-|

    import Document exposing(Document, dummy, UserPermission(..), Permission(..))
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility
    import Json.Decode as Decode


    uuid1 : Uuid
    uuid1 =
        Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
            |> Maybe.withDefault Utility.id0

    testDocument1 : Document
    testDocument1 =  {dummy | tags = ["a", "b"], slug = Document.makeSlug dummy, childInfo = [ ( uuid1, 1 ) ], permissions = [] }


    testDocument : Document
    testDocument =  {dummy | tags = ["a", "b"], slug = Document.makeSlug dummy, childInfo = [ ( uuid1, 1 ) ], permissions = [UserPermission "jxx" WritePermission] }

    encodeDocument testDocument
    -->  "{\n    \"id\": \"59ddd53f-951f-4331-bd5b-95bdfb9b3113\",\n    \"title\": \"dummy\",\n    \"authorIdentifier\": \"bozo\",\n    \"content\": \"nothing here\",\n    \"public\": true,\n    \"tags\": [\n        \"a\",\n        \"b\"\n    ],\n    \"slug\": \"bozo.dummy.9b31\",\n    \"docType\": \"MiniLaTeX\",\n    \"childInfo\": [\n        \"(3db857d2-1422-47a9-8f04-4fc6efe871cc,1)\"\n    ],\n    \"permissions\": [\n        \"(jxx, w)\"\n    ]\n}"

    Decode.decodeString documentDecoder (encodeDocument testDocument1)
    --> Ok testDocument1

-}
encodeDocument : Document -> String
encodeDocument doc =
    Encode.encode 4 (documentEncoder doc)


{-|

    import Document exposing(Document, dummy, UserPermission(..), Permission(..))
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility
    import Json.Decode as Decode


    uuid1 : Uuid
    uuid1 =
        Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
            |> Maybe.withDefault Utility.id0

    testDocument1 : Document
    testDocument1 =  {dummy | tags = ["a", "b"], slug = Document.makeSlug dummy, childInfo = [ ( uuid1, 1 ) ], permissions = [] }


    testDocument : Document
    testDocument =  {dummy | tags = ["a", "b"], slug = Document.makeSlug dummy, childInfo = [ ( uuid1, 1 ) ], permissions = [UserPermission "jxx" WritePermission] }

    Decode.decodeString documentListDecoder (encodeDocumentList [testDocument, testDocument1])
    --> Ok [testDocument, testDocument1]

-}
encodeDocumentList : List Document -> String
encodeDocumentList list =
    Encode.encode 4 (documentListEncoder list)


documentListEncoder : List Document -> Value
documentListEncoder list =
    Encode.list documentEncoder list


documentEncoder : Document -> Value
documentEncoder doc =
    Encode.object
        [ ( "id", Encode.string (Uuid.toString doc.id) )
        , ( "title", Encode.string doc.title )
        , ( "authorIdentifier", Encode.string doc.authorIdentifier )
        , ( "content", Encode.string doc.content )
        , ( "public", Encode.bool doc.public )
        , ( "tags", Encode.list Encode.string doc.tags )
        , ( "slug", Encode.string doc.slug )
        , ( "docType", Encode.string (doc.docType |> Document.stringFromDocType) )
        , ( "childInfo", Encode.list encodeChildInfoItem doc.childInfo )
        , ( "permissions", Encode.list encodePermission doc.permissions )
        ]


encodePermission : UserPermission -> Value
encodePermission up =
    up
        |> Document.stringFromUserPermission
        |> Encode.string


encodeChildInfoItem : ( Uuid, Int ) -> Value
encodeChildInfoItem item =
    item
        |> Document.stringFromChildInfoItem
        |> Encode.string



-- DECODER --


documentListDecoder : Decoder (List Document)
documentListDecoder =
    Decode.list documentDecoder


documentDecoder : Decoder Document
documentDecoder =
    Decode.succeed Document
        |> required "id" decoderUuid
        |> required "title" Decode.string
        |> required "authorIdentifier" Decode.string
        |> required "content" Decode.string
        |> required "public" Decode.bool
        |> required "tags" (Decode.list Decode.string)
        |> required "slug" Decode.string
        |> required "docType" (Decode.string |> Decode.map (adjustDocTypeString >> Document.docTypeFromString))
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
            Decode.fail "Bad string for user permission"


getPermission : String -> Maybe UserPermission
getPermission str =
    Parser.run parseUserPermission str
        |> Result.toMaybe


adjustDocTypeString : String -> String
adjustDocTypeString str =
    case str of
        "minilatex" ->
            "MiniLaTeX"

        "mdstandard" ->
            "MDStandard"

        "mdextended" ->
            "MDExtended"

        "mdeExtendedMath" ->
            "MDExtendedMath"

        _ ->
            str


decodeChildInfoItem : Decoder ( Uuid, Int )
decodeChildInfoItem =
    Decode.string
        |> Decode.andThen decoderChildInfoItemAux


decoderChildInfoItemAux : String -> Decoder ( Uuid, Int )
decoderChildInfoItemAux str =
    str
        |> Parser.run childInfoItemAuxParser
        |> handleItemParseResult


handleItemParseResult : Result (List Parser.DeadEnd) ( Uuid, Int ) -> Decoder ( Uuid, Int )
handleItemParseResult result =
    case result of
        Ok pair ->
            Decode.succeed pair

        Err _ ->
            Decode.fail "Error parsing pair"


decoderUuid : Decoder Uuid
decoderUuid =
    Decode.string
        |> Decode.andThen decoderIdAux


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



-- PARSING --


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



-- TEST DATA --


uuid1 : Uuid
uuid1 =
    Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
        |> Maybe.withDefault Utility.id0
