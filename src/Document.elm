module Document exposing
    ( DocType(..)
    , Document
    , MarkdownFlavor(..)
    , create
    , deleteChild
    , docTypeFromString
    , footer
    , getById
    , getContent
    , getDocType
    , getLevel
    , idAndTitleList
    , insertDocumentInList
    , level
    , makeTocStatus
    , reOrder
    , reorderChildren
    , replaceInList
    , setContent
    , setLevelsOfChildren
    , sortChildren
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
    , childLevels : List Int
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


makeTocStatus : Document -> TocStatus
makeTocStatus document =
    let
        data =
            List.map2 (\x y -> ( x, y )) document.children document.childLevels

        status : ( Uuid, Int ) -> ( Uuid, Bool )
        status ( uuid, level_ ) =
            if level_ == 0 then
                ( uuid, True )

            else
                ( uuid, False )
    in
    List.map status data


{-| Get level of document as defined by master
-}
getLevel : Document -> Document -> Int
getLevel master document =
    let
        idx : Int
        idx =
            List.Extra.elemIndex document.id master.children |> Maybe.withDefault -1
    in
    List.Extra.getAt idx master.childLevels |> Maybe.withDefault -1


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


reOrder : List String -> List ( Uuid, String ) -> List Uuid
reOrder strList annotatedList =
    let
        order : String -> Int
        order str =
            List.Extra.elemIndex str strList |> Maybe.withDefault -1
    in
    List.sortBy (\( uuid, str ) -> order str) annotatedList
        |> List.map Tuple.first


reorderChildren : Document -> List String -> List Document -> Document
reorderChildren masterDocument titleList childDocumentList =
    let
        childUuidList =
            List.map .id childDocumentList

        childTitleList =
            List.map .title childDocumentList

        annotatedList =
            List.map2 (\u v -> ( u, v )) childUuidList childTitleList

        newUuidList =
            reOrder titleList annotatedList
    in
    case equalUuidLists newUuidList masterDocument.children of
        True ->
            { masterDocument | children = newUuidList }

        False ->
            masterDocument


equalUuidLists : List Uuid -> List Uuid -> Bool
equalUuidLists l1 l2 =
    List.sortBy Uuid.toString l1 == List.sortBy Uuid.toString l2


setLevelsOfChildren : String -> Document -> Document
setLevelsOfChildren outline master =
    let
        newChildLevels =
            outline
                |> String.split "\n"
                |> List.filter (\item -> item /= "")
                |> List.map level
    in
    { master | childLevels = newChildLevels }


sortChildren : Document -> List Document -> List Document
sortChildren master list =
    let
        idList =
            master.id :: master.children

        order : Document -> Int
        order doc =
            List.Extra.elemIndex doc.id idList |> Maybe.withDefault -1
    in
    List.sortBy order list


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
        ++ String.fromInt (List.length document.children)
        ++ "\n"
        ++ "Levels: "
        ++ String.fromInt (List.length document.childLevels)
        --        ++ "\n"
        --        ++ "\nSummary: "
        --        ++ childrenSummary document
        ++ "\n\n"
        ++ makeSlug document
        ++ "\n"
        ++ (document.id |> Uuid.toString)
        ++ "\n"
        ++ "\n\n"


childrenSummary : Document -> String
childrenSummary document =
    document.children
        |> List.map Uuid.toString
        |> List.map (String.right 4)
        |> String.join ", "


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
    , childLevels = []
    , childInfo = []
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
