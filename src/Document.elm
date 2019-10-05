module Document exposing
    ( DocType(..)
    , Document
    , MarkdownFlavor(..)
    , create
    , docTypeFromString
    , footer
    , getById
    , getContent
    , getDocType
    , replaceInList
    , setContent
    , stringFromDocType
    , updateMetaData
    )

import List.Extra
import Parser exposing ((|.), Parser, chompWhile, getChompedString, succeed, symbol)
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (Posix)
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
    , children : List Uuid
    }


type DocType
    = Markdown MarkdownFlavor
    | MiniLaTeX
    | Collection


type MarkdownFlavor
    = MDStandard
    | MDExtended
    | MDExtendedMath


getDocType : Maybe Document -> DocType
getDocType maybeDocument =
    case maybeDocument of
        Nothing ->
            Markdown MDExtendedMath

        Just doc ->
            doc.docType


docTypeFromString : String -> DocType
docTypeFromString str =
    case str of
        "MiniLaTeX" ->
            MiniLaTeX

        "Collection" ->
            Collection

        "MDStandard" ->
            Markdown MDStandard

        "MDExtended" ->
            Markdown MDExtended

        "MDExtendedMath" ->
            Markdown MDExtendedMath

        _ ->
            Markdown MDExtendedMath


stringFromDocType : DocType -> String
stringFromDocType docType =
    case docType of
        MiniLaTeX ->
            "MiniLaTeX"

        Collection ->
            "Collection"

        Markdown flavor ->
            case flavor of
                MDStandard ->
                    "MDStandard"

                MDExtended ->
                    "MDExtended"

                MDExtendedMath ->
                    "MDExtendedMath"


setContent : String -> Document -> Document
setContent str document =
    { document | content = str }


getContent : Maybe Document -> String
getContent maybeDocument =
    case maybeDocument of
        Just document ->
            document.content

        Nothing ->
            ""


getById : String -> List Document -> Maybe Document
getById str docList =
    case Uuid.fromString str of
        Nothing ->
            Nothing

        Just uuid ->
            List.filter (\doc -> doc.id == uuid) docList |> List.head


footer : Document -> String
footer document =
    "Author: "
        ++ document.authorIdentifier
        ++ "\n"
        ++ "Public: "
        ++ Utility.boolAsString document.public
        ++ "\n"
        ++ "Tags: "
        ++ String.join ", " document.tags
        ++ "\n"
        ++ "Words: "
        ++ Utility.wordCount document.content
        ++ "\n"
        ++ "Document type: "
        ++ (document.docType |> stringFromDocType)
        ++ "\n"
        ++ "\n"
        ++ makeSlug document
        ++ "\n"
        ++ (document.id |> Uuid.toString)
        ++ "\n"
        ++ "\n\n"


makeSlug : Document -> String
makeSlug document =
    let
        endOfHash =
            Uuid.toString document.id |> String.right 6

        shortHash =
            String.left 3 endOfHash ++ "-" ++ String.right 3 endOfHash
    in
    document.authorIdentifier ++ "." ++ Utility.compress document.title ++ "." ++ shortHash


makeInitialSlug : String -> String -> Uuid -> String
makeInitialSlug title authorIdentifier identifier =
    let
        endOfHash =
            Uuid.toString identifier |> String.right 6

        shortHash =
            String.left 3 endOfHash ++ "-" ++ String.right 3 endOfHash
    in
    authorIdentifier ++ "." ++ Utility.compress title ++ "." ++ shortHash


{-|

    > create "jxxcarlson" "Intro to Chromaticity" t "First draft ..."
    --> {   authorIdentifier = "jxxcarlson", children = [], content = "First draft ..."
          , id = "jxxcarlson.intro-to-chromaticity.1568667528"
          , public = False, tags = [], timeCreated = Posix 1568667528000
          , timeUpdated = Posix 1568667528000, title = "Intro to Chromaticity" }
        : Document

-}
create : Uuid -> String -> String -> String -> Document
create documentUuid authorIdentifier title content =
    let
        slug =
            makeInitialSlug title authorIdentifier documentUuid
    in
    { id = documentUuid
    , title = title
    , authorIdentifier = authorIdentifier
    , content = content
    , tags = []
    , public = False
    , slug = slug
    , docType = Markdown MDExtendedMath
    , children = []
    }


{-| Replace by the target document any occurrence of a document in
the documentList whose id is the same as that of the target document.
It is assumed, but not enforced, that document ids are unique.
-}
replaceInList : Document -> List Document -> List Document
replaceInList targetDocument documentList =
    --   let
    --       targetDocument = updateMetaData targetDocument_
    --    in
    List.Extra.setIf (\doc -> doc.id == targetDocument.id) targetDocument documentList


updateMetaData : Document -> Document
updateMetaData document =
    { document | slug = makeSlug document, title = newTitle document }


newTitle : Document -> String
newTitle document =
    case getHeading document of
        Nothing ->
            document.title

        Just newTitle_ ->
            newTitle_


getHeading : Document -> Maybe String
getHeading document =
    case Parser.run parseHeading document.content of
        Ok result ->
            String.dropLeft 1 result |> String.trim |> Just

        Err _ ->
            Nothing


parseHeading : Parser String
parseHeading =
    (getChompedString <|
        succeed identity
            |. parseWhile (\c -> c /= '#')
            |. symbol "#"
            |. symbol " "
            |. parseWhile (\c -> c /= '\n')
    )
        |> Parser.map String.trim


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString
