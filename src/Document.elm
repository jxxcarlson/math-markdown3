module Document exposing
    ( DocType(..)
    , Document
    , MarkdownFlavor(..)
    , create
    , deleteChild
    , docTypeFromString
    , encodeStringList
    , footer
    , getById
    , getContent
    , getDocType
    , idAndTitleList
    , idList
    , idListOfDeque
    , insertDocumentInList
    , level
    , makeTocStatus
    , pushFrontUnique
    , reOrder
    , reorderChildrenInMaster
    , replaceInList
    , setContent
    , sortChildren
    , stringFromDocType
    , totalWordCount
    , updateMetaData
    , uuidListDecoder
    , uuidListFromStrings
    )

import BoundedDeque exposing (BoundedDeque)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), Parser, chompWhile, getChompedString, succeed, symbol)
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
    }


type DocType
    = Markdown MarkdownFlavor
    | MiniLaTeX
    | Collection


type MarkdownFlavor
    = MDStandard
    | MDExtended
    | MDExtendedMath


{-| Used to determine whether a toc entry
displays its subdocuments or not
-}
type alias TocStatus =
    List ( Uuid, Bool )


idList : Document -> List Uuid
idList document =
    document.childInfo
        |> List.map Tuple.first


makeTocStatus : Document -> TocStatus
makeTocStatus document =
    let
        status : ( Uuid, Int ) -> ( Uuid, Bool )
        status ( uuid, level_ ) =
            if level_ == 0 then
                ( uuid, True )

            else
                ( uuid, False )
    in
    List.map status document.childInfo


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


insertDocumentInList : Document -> Document -> List Document -> List Document
insertDocumentInList newDocument targetDocument documentList =
    let
        equal : Document -> Document -> Bool
        equal x y =
            x.id == y.id
    in
    Utility.insertItemInList equal newDocument targetDocument documentList


idAndTitleList : List Document -> List ( Uuid, String )
idAndTitleList list =
    List.map (\doc -> ( doc.id, doc.title )) list


reOrder : List String -> List ( String, a ) -> List ( String, a )
reOrder titleList annotatedList =
    let
        order : String -> Int
        order str =
            List.Extra.elemIndex str titleList |> Maybe.withDefault -1
    in
    List.sortBy (\( str, _ ) -> order str) annotatedList


{-| Reorder children in master given given (1) the list of
curretnt child document titles, (2) a different ordering of
the titles.

Assumption: the two lists do not refer at all to master

Safety: p

-}
reorderChildrenInMaster : Document -> List String -> List String -> Document
reorderChildrenInMaster masterDocument childTitleList newChildTitleList =
    let
        annotatedList =
            List.map2 (\u v -> ( u, v )) childTitleList masterDocument.childInfo

        newChildInfo =
            reOrder newChildTitleList annotatedList
                |> List.map Tuple.second
    in
    case equalUuidSets newChildInfo masterDocument.childInfo of
        True ->
            { masterDocument | childInfo = newChildInfo }

        False ->
            masterDocument


equalUuidSets : List ( Uuid, a ) -> List ( Uuid, a ) -> Bool
equalUuidSets l1 l2 =
    let
        order =
            \item -> Uuid.toString (Tuple.first item)
    in
    List.sortBy order l1 == List.sortBy order l2


{-| Assume that the master is not part of the document list
-}
sortChildren : Document -> List Document -> List Document
sortChildren master documentList =
    let
        idList_ =
            master.id :: (master.childInfo |> List.map Tuple.first)

        order : Document -> Int
        order doc =
            List.Extra.elemIndex doc.id idList_ |> Maybe.withDefault -1
    in
    List.sortBy order documentList


deleteChild : Document -> Document -> Document
deleteChild documentToDelete masterDocument =
    { masterDocument | childInfo = List.filter (\item -> Tuple.first item /= documentToDelete.id) masterDocument.childInfo }


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
        ++ "Children: "
        ++ String.fromInt (List.length document.childInfo)
        ++ "\n\n"
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
    , childInfo = []
    }


pushFrontUnique : Document -> BoundedDeque Document -> BoundedDeque Document
pushFrontUnique doc deque =
    deque
        |> BoundedDeque.filter (\d -> d.id /= doc.id)
        |> BoundedDeque.pushFront doc


idListOfDeque : BoundedDeque Document -> List String
idListOfDeque deque =
    deque
        |> BoundedDeque.toList
        |> List.map (.id >> Uuid.toString)


uuidListFromStrings : List String -> List Uuid
uuidListFromStrings strList =
    strList
        |> List.map Uuid.fromString
        |> Maybe.Extra.values


encodeStringList : String -> List String -> E.Value
encodeStringList name strList =
    E.object
        [ ( name, E.list E.string strList ) ]


uuidListDecoder : Decoder (List Uuid)
uuidListDecoder =
    D.field "deque" (D.list D.string) |> D.map (List.map Uuid.fromString) |> D.map Maybe.Extra.values


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



-- PARSING
-- PARSE LEVEL


parseLeadingSpace : Parser String
parseLeadingSpace =
    getChompedString <|
        succeed identity
            |. parseWhile (\c -> c == ' ')


level : String -> Int
level str =
    case Parser.run parseLeadingSpace str of
        Ok leadingSpace ->
            String.length leadingSpace // 3

        _ ->
            0



-- PARSE HEADING


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


totalWordCount : List Document -> Int
totalWordCount documentList =
    documentList
        |> List.map (.content >> String.words >> List.length)
        |> List.sum
