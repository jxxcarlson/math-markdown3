module Interchange exposing (childInfoItemAuxParser, decodeChildInfoItem, decoderUuid, documentDecoder, encodeDocument, parseStringToChar, uuidParser)

import Document exposing (DocType(..), Document, UserPermission(..), dummy)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), Parser)
import Prng.Uuid as Uuid exposing (Uuid)
import Utility


uuid1 : Uuid
uuid1 =
    Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
        |> Maybe.withDefault Utility.id0


{-|

    import Document exposing(dummy, UserPermission(..), Permission(..))
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility

    uuid1 : Uuid
    uuid1 =
        Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
            |> Maybe.withDefault Utility.id0

    encodeDocument {dummy | tags = ["a", "b"], slug = Document.makeSlug dummy, childInfo = [ ( uuid1, 1 ) ], permissions = [UserPermission "jxx" WritePermission] }
    -->  "{\n    \"id\": \"59ddd53f-951f-4331-bd5b-95bdfb9b3113\",\n    \"title\": \"dummy\",\n    \"authorIdentifier\": \"bozo\",\n    \"content\": \"nothing here\",\n    \"public\": true,\n    \"tags\": [\n        \"a\",\n        \"b\"\n    ],\n    \"slug\": \"bozo.dummy.9b31\",\n    \"docType\": \"MiniLaTeX\",\n    \"childInfo\": \"[(3db857d2-1422-47a9-8f04-4fc6efe871cc,1)]\",\n    \"permissions\": \"[(jxx, w)]\"\n}"

-}
encodeDocument : Document -> String
encodeDocument doc =
    Encode.encode 4 (documentEncoder doc)


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
        , ( "childInfo", Encode.string (doc.childInfo |> Document.stringFromChildInfo) )
        , ( "permissions", Encode.string (doc.permissions |> Document.stringFromPermissions) )
        ]


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
            Decode.fail "Bad string for user permissin"


getPermission : String -> Maybe UserPermission
getPermission str =
    Parser.run parseUserPermission str
        |> Result.toMaybe


adjustDocTypeString : String -> String
adjustDocTypeString str =
    case Debug.log "Str" str of
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


{-|

    -- XXX

    import Prng.Uuid as Uuid exposing (Uuid)
    import Json.Decode as Decode
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    Decode.fromString decodeChildInfoItem "(3db857d2-1422-47a9-8f04-4fc6efe871cc:8)"
    -> Ok (uuid, 8)

    Decode.fromString (Decode.list decodeChildInfoItem) ("[]")
    -> Ok ([])

    Decode.fromString (Decode.list decodeChildInfoItem) ("[(3db857d2-1422-47a9-8f04-4fc6efe871cc:1),(3db857d2-1422-47a9-8f04-4fc6efe871cc:2)]")
    XXX-> Ok ([(uuid, 1),(uuid,2)])

-}
decodeChildInfoItem : Decoder ( Uuid, Int )
decodeChildInfoItem =
    Decode.string
        |> Decode.andThen decoderChildInfoItemAux


{-|

    import Json.Decode as Decode exposing (Decoder)
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
            |> Maybe.withDefault Utility.id0

    -- Decode.fromString decodeChildInfoItem "(3db857d2-1422-47a9-8f04-4fc6efe871cc:88)"
      x-> Ok (uuid, 88)

-}
decoderChildInfoItemAux : String -> Decoder ( Uuid, Int )
decoderChildInfoItemAux str =
    str
        |> Debug.log "Parser input (1)"
        |> Parser.run childInfoItemAuxParser
        |> handleItemParseResult


handleItemParseResult : Result (List Parser.DeadEnd) ( Uuid, Int ) -> Decoder ( Uuid, Int )
handleItemParseResult result =
    case Debug.log "hpr (1)" result of
        Ok pair ->
            Decode.succeed pair

        Err _ ->
            Decode.fail "Error parsing pair"


{-|

    import Parser
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    Parser.run childInfoItemAuxParser ("(" ++ uuidString ++ " : 4)")
    --> Ok (uuid,4)

    Parser.run childInfoItemAuxParser "(3db857d2-1422-47a9-8f04-4fc6efe871cc:7)"
    --> Ok (uuid,7 )

-}
childInfoItemAuxParser : Parser ( Uuid, Int )
childInfoItemAuxParser =
    Parser.succeed (\uuid k -> ( uuid, Debug.log "KK" k ))
        |. Parser.symbol "("
        |. Parser.spaces
        |= (uuidParser |> Debug.log "UUU")
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ")"


{-|

    import Prng.Uuid as Uuid exposing (Uuid)
    import Json.Decode as Decode
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    -- Decode.fromString decoderUuid uuidString
    -> Ok uuid

-}
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


{-|

    import Parser

    Parser.run (parseStringToChar '.') "test."
    --> Ok "test"

-}
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


{-|

    import Parser
    import Prng.Uuid as Uuid exposing (Uuid)
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    Parser.run uuidParser uuidString
    --> Ok uuid

-}
uuidParser : Parser Uuid
uuidParser =
    parseStringToChar ':' |> Parser.andThen uuidParserAux


uuidParserAux : String -> Parser Uuid
uuidParserAux str =
    case Uuid.fromString str of
        Just uuid ->
            Parser.succeed uuid

        Nothing ->
            Parser.problem "Bad uuid string"
