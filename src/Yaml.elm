module Yaml exposing (childInfoItemAuxParser, decodeChildInfoItem, decodeItem, decoderChildInfoItemAux, decoderId, decoderUuid, dummy1, dummy2, foo, fromDocument, fromDocumentList, parseStringToChar, required, toDocument, toDocumentList, uuidParser, uuidParserAux)

import Document exposing (DocType(..), Document, Permission(..), UserPermission(..))
import Parser exposing ((|.), (|=), Parser)
import Prng.Uuid as Uuid exposing (Uuid)
import Utility
import Yaml.Decode as Decode exposing (Decoder)



--{-|
--
--    import Yaml.Decode as Decode exposing (Decoder)
--
--    type alias Foo =
--        { bar : String
--        , baz : Int
--        }
--
--    fooDecoder : Decoder Foo
--    fooDecoder =
--       Decode.succeed Foo
--            |> required "bar" Decode.string
--            |> required "baz" Decode.int
--
--    str : String
--    str =
--        """   bar: hohoho!
--       baz: 43
--    """
--
--    Decode.fromString fooDecoder str
--    --> Ok { bar = "hohoho!", baz = 43 }
--
---}


type alias Docu =
    { id : Uuid
    , title : String
    , authorIdentifier : String
    , content : String
    , public : Bool
    , tags : List String
    , slug : String
    , docType : DocType
    , childInfo : List ( Uuid, Int )

    --    , permissions : List UserPermission
    }


uuid1 : Uuid
uuid1 =
    Uuid.fromString "3db857d2-1422-47a9-8f04-4fc6efe871cc"
        |> Maybe.withDefault Utility.id0


dummy1 : Docu
dummy1 =
    { id = Utility.getId 1
    , title = "dummy1"
    , authorIdentifier = "bozo"
    , content = "nothing\nmore\nthan\nthis"
    , public = True
    , tags = []
    , slug = "dummy.dummy"
    , docType = MiniLaTeX
    , childInfo = [ ( uuid1, 1 ) ]

    --    , permissions = []
    }


dummy2 : Docu
dummy2 =
    { id = Utility.getId 1
    , title = "dummy2"
    , authorIdentifier = "bozo"
    , content = "nothing\nmore\nthan\nthis"
    , public = True
    , tags = [ "alpha", "beta" ]
    , slug = "dummy.dummy"
    , docType = MiniLaTeX
    , childInfo = [ ( uuid1, 1 ) ]

    --    , permissions = []
    }


{-| Output validated at <https://codebeautify.org/yaml-validator>
-}
fromDocumentList : List Docu -> String
fromDocumentList noteList =
    "---\n"
        ++ List.foldl (\document acc -> fromDocument document ++ acc) "" noteList


{-|

    -- import Data.TestDocs exposing(dummy, master)

    fromDocument dummy1
    -->  "   id: 59ddd53f-951f-4331-bd5b-95bdfb9b3113\n   title: dummy1\n   authorIdentifier: bozo\n   content:       nothing\n      more\n      than\n      this\n   public: True\n   tags: []\n   slug: dummy.dummy\n   docType: MiniLaTeX\n   childInfo: [(3db857d2-1422-47a9-8f04-4fc6efe871cc:1)]\n"

-}
fromDocument : Docu -> String
fromDocument document =
    "   id: "
        ++ Uuid.toString document.id
        ++ "\n"
        ++ "   title: "
        ++ document.title
        ++ "\n"
        ++ "   authorIdentifier: "
        ++ document.authorIdentifier
        ++ "\n"
        ++ "   content: "
        ++ (document.content |> prepareMultiline)
        ++ "\n"
        ++ "   public: "
        ++ (document.public |> stringFromBool)
        ++ "\n"
        ++ "   tags: ["
        ++ String.join ", " document.tags
        ++ "]\n"
        ++ "   slug: "
        ++ document.slug
        ++ "\n"
        ++ "   docType: "
        ++ (document.docType |> Document.stringFromDocType)
        ++ "\n"
        ++ "   childInfo: "
        ++ (document.childInfo |> Document.stringFromChildInfo)
        ++ "\n"



--        ++ "   permissions: "
--        ++ (document.permissions |> Document.stringFromPermissions)
--        ++ "\n"


prepareMultiline : String -> String
prepareMultiline s =
    s
        |> String.lines
        |> List.map (\ss -> "      " ++ ss)
        |> String.join "\n"


toDocumentList : String -> Maybe (List Docu)
toDocumentList str =
    Decode.fromString documentListDecoder str
        |> Result.toMaybe


{-|

    -- XXX

    toDocument (fromDocument dummy2)
    -> Just dummy2

    toDocument (fromDocument dummy1)
    -> Just dummy1

    toDocument "   id: 59ddd53f-951f-4331-bd5b-95bdfb9b3113\n   title: dummy\n   authorIdentifier: bozo\n   content:       nothing\n      more\n      than\n      this\n   public: True\n   tags: []\n   slug: dummy.dummy\n   docType: MiniLaTeX\n   childInfo: [(3db857d2-1422-47a9-8f04-4fc6efe871cc, 1)]\n"
    -> Just dummy1

-}
toDocument : String -> Maybe Docu
toDocument str =
    Decode.fromString documentDecoder str
        |> Result.toMaybe



-- DECODERS --


documentListDecoder : Decoder (List Docu)
documentListDecoder =
    Decode.list documentDecoder


documentDecoder : Decoder Docu
documentDecoder =
    Decode.succeed Docu
        |> required "id" decoderUuid
        |> required "title" Decode.string
        |> required "authorIdentifier" Decode.string
        |> required "content" Decode.string
        |> required "public" Decode.bool
        |> required "tags" (Decode.list Decode.string)
        |> required "slug" Decode.string
        |> required "docType" (Decode.string |> Decode.map (adjustDocTypeString >> Document.docTypeFromString))
        -- |> required "childInfo" (Decode.succeed [ ( uuidX, 1 ) ])
        |> required "childInfo" (Decode.list decodeChildInfoItem)



-- (Decode.list decodeChildInfoItem)
--        |> required "permissions" (Decode.list decodePermission)


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


uuidStringX : String
uuidStringX =
    "3db857d2-1422-47a9-8f04-4fc6efe871cc"


uuidX : Uuid
uuidX =
    Uuid.fromString uuidStringX
        |> Maybe.withDefault Utility.id0


{-|

    -- XXX

    import Prng.Uuid as Uuid exposing (Uuid)
    import Yaml.Decode as Decode
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



{-

   -- XXX

   import Prng.Uuid as Uuid exposing (Uuid)
   import Yaml.Decode as Decode
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


{-|

    import Yaml.Decode as Decode exposing (Decoder)

    Decode.fromString decodeItem "(Test:88)"
    --> Ok ("Test", 88)

-}
decodeItem : Decoder ( String, Int )
decodeItem =
    Decode.string
        |> Decode.andThen baz


baz : String -> Decoder ( String, number )
baz =
    \s -> Decode.succeed ( "Test", 88 )



-- decoderChildInfoItemAux


{-|

    import Prng.Uuid as Uuid exposing (Uuid)
    import Yaml.Decode as Decode exposing(Decoder)
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    Decode.fromString decodeChildInfoItem "(3db857d2-1422-47a9-8f04-4fc6efe871cc:88)"
    --> Ok (uuid, 88)

-}
decoderChildInfoItemAux : String -> Decoder ( Uuid, Int )
decoderChildInfoItemAux str =
    str
        |> Debug.log "Parser input (1)"
        |> Parser.run fakeChildInfoItemAuxParser
        |> handleItemParseResult



--foo : String -> Result (List Parser.DeadEnd) ( Uuid, number )
--foo str =
--    Parser.run (Parser.succeed ( uuidX, 55 ))


foo =
    1


fakeChildInfoItemAuxParser : Parser ( Uuid, number )
fakeChildInfoItemAuxParser =
    Parser.succeed ( uuidX, 55 )


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
    import Yaml.Decode as Decode
    import Utility

    uuidString : String
    uuidString =
        "3db857d2-1422-47a9-8f04-4fc6efe871cc"

    uuid : Uuid
    uuid =
        Uuid.fromString uuidString
        |> Maybe.withDefault Utility.id0

    Decode.fromString decoderUuid uuidString
    --> Ok uuid

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
