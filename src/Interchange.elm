module Interchange exposing (Document, encodeDocument)

import Document exposing (DocType(..), Document, UserPermission(..), dummy)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Utility


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
