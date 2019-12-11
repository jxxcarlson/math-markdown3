module Document exposing
    ( DocType(..)
    , Document
    , DocumentError(..)
    , MarkdownFlavor(..)
    , Permission(..)
    , UserPermission(..)
    , addPermission
    , create
    , deleteChild
    , docTypeFromString
    , dummy
    , editable
    , elementsAreUnique
    , encodeStringList
    , equalUuidSets
    , footer
    , getById
    , getContent
    , getDocType
    , idAndTitleList
    , idList
    , idListOfDeque
    , insertDocumentInList
    , level
    , listPermissions
    , makeSlug
    , makeTocStatus
    , permissionFromString
    , permissionToString
    , pushFrontUnique
    , reOrder
    , readPermissionForUserAsJsonString
    , reorderChildrenInMaster
    , replaceInList
    , setContent
    , sortChildren
    , stringFromChildInfo
    , stringFromDocType
    , stringFromPermissions
    , stringOfError
    , totalWordCount
    , updateMetaData
    , userPermissionToString
    , uuidListDecoder
    , uuidListFromStrings
    , writePermissionForUserAsJsonString
    )

import BoundedDeque exposing (BoundedDeque)
import DocParser
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra
import Maybe.Extra
import Prng.Uuid as Uuid exposing (Uuid)
import String.Interpolate exposing (interpolate)
import Utility
import Utility.List
import Utility.String


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


dummy : Document
dummy =
    { id = Utility.getId 1
    , title = "dummy"
    , authorIdentifier = "bozo"
    , content = "nothing here"
    , public = True
    , tags = []
    , slug = "dummy.dummy"
    , docType = MiniLaTeX
    , childInfo = []
    , permissions = []
    }


type Permission
    = ReadPermission
    | WritePermission
    | NoPermission


type UserPermission
    = UserPermission String Permission


getPermission : String -> List UserPermission -> UserPermission
getPermission username permissions =
    permissions
        |> List.filter (\p -> userNameOfUserPermission p == username)
        |> List.head
        |> Maybe.withDefault (UserPermission "_nobody_" NoPermission)


editable : String -> Document -> Bool
editable username doc =
    if username == doc.authorIdentifier then
        True

    else
        userCanWrite_ username doc.permissions


userCanWrite_ : String -> List UserPermission -> Bool
userCanWrite_ username permissions =
    let
        (UserPermission targetName targetPermission) =
            getPermission username permissions
    in
    case ( targetName == username, targetPermission == WritePermission ) of
        ( True, True ) ->
            True

        ( _, _ ) ->
            False


readPermissionForUserAsJsonString : String -> String
readPermissionForUserAsJsonString username =
    "{" ++ username ++ ":ReadPermission}"


writePermissionForUserAsJsonString : String -> String
writePermissionForUserAsJsonString username =
    "{" ++ username ++ ":WritePermission}"


listPermissions : Document -> List String
listPermissions doc =
    doc.permissions
        |> List.map (\p -> userPermissionToString2 p)


stringOfUserPermission : UserPermission -> String
stringOfUserPermission up =
    userNameOfUserPermission up ++ ": " ++ userPermissionToString up


userNameOfUserPermission : UserPermission -> String
userNameOfUserPermission (UserPermission username _) =
    username


permissionOfUserPermission : UserPermission -> Permission
permissionOfUserPermission (UserPermission _ permission) =
    permission


addPermission : String -> Permission -> List UserPermission -> List UserPermission
addPermission username permission userPermissionList =
    let
        cleanPermissionList =
            List.filter (\p -> userNameOfUserPermission p /= username) userPermissionList

        newPermission =
            UserPermission username permission
    in
    newPermission :: cleanPermissionList


userPermissionToString : UserPermission -> String
userPermissionToString (UserPermission username permission) =
    "{" ++ username ++ ":" ++ permissionToString permission ++ "}"


userPermissionToString2 : UserPermission -> String
userPermissionToString2 (UserPermission username permission) =
    username ++ ": " ++ permissionToString permission


permissionToString : Permission -> String
permissionToString p =
    case p of
        ReadPermission ->
            "ReadPermission"

        WritePermission ->
            "WritePermission"

        NoPermission ->
            "NoPermission"


type DocType
    = Markdown MarkdownFlavor
    | MiniLaTeX


type MarkdownFlavor
    = MDStandard
    | MDExtended
    | MDExtendedMath


type DocumentError
    = RepeatedListElements
    | ListsOfDifferentLengths
    | ListsOfDifferentLengthsTM1
    | ListsOfDifferentLengthsTM2 String
    | UnequalSets
    | UuidListsDoNotMatch
    | DocumentListIsEmpty


stringOfError : DocumentError -> String
stringOfError docError =
    case docError of
        RepeatedListElements ->
            "Repeated list elements"

        ListsOfDifferentLengths ->
            "Lists of different lengths"

        ListsOfDifferentLengthsTM1 ->
            "Lists of different lengths (tm1)"

        ListsOfDifferentLengthsTM2 str ->
            "ListsOfDifferentLengthsTM2 " ++ str

        UnequalSets ->
            "Undqual sets"

        UuidListsDoNotMatch ->
            "Uuid lists don't  match'"

        DocumentListIsEmpty ->
            "Document list is emptys="


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

        Markdown flavor ->
            case flavor of
                MDStandard ->
                    "MDStandard"

                MDExtended ->
                    "MDExtended"

                MDExtendedMath ->
                    "MDExtendedMath"


stringFromChildInfo : List ( Uuid, Int ) -> String
stringFromChildInfo uuidList =
    uuidList
        |> List.map (\( uuid, k ) -> interpolate "({0},{1})" [ Uuid.toString uuid, String.fromInt k ])
        |> String.join ", "
        |> (\s -> "[" ++ s ++ "]")


permissionFromString : String -> Permission
permissionFromString str =
    case str of
        "r" ->
            ReadPermission

        "w" ->
            WritePermission

        _ ->
            NoPermission


stringFromPermissions : List UserPermission -> String
stringFromPermissions permissionList =
    let
        stringFromPermission p =
            case p of
                ReadPermission ->
                    "r"

                WritePermission ->
                    "w"

                NoPermission ->
                    "n"

        stringFromUserPermission : UserPermission -> String
        stringFromUserPermission (UserPermission user p) =
            interpolate "({0}, {1})" [ user, stringFromPermission p ]
    in
    permissionList
        |> List.map stringFromUserPermission
        |> String.join ", "
        |> (\s -> interpolate "[{0}]" [ s ])


insertDocumentInList : Document -> Document -> List Document -> List Document
insertDocumentInList newDocument targetDocument documentList =
    let
        equal : Document -> Document -> Bool
        equal x y =
            x.id == y.id
    in
    Utility.List.insertItem equal newDocument targetDocument documentList


idAndTitleList : List Document -> List ( Uuid, String )
idAndTitleList list =
    List.map (\doc -> ( doc.id, doc.title )) list


elementsAreUnique : List comparable -> Bool
elementsAreUnique list =
    List.length list == List.length (List.Extra.unique list)


{-| Reorder the annotatedList so that its projection onto the
first component is the same as the titleList

    titleList : List String
    titleList =
        [ "A", "C", "B" ]

    annotatedList : List (String, Int)
    annotatedList =
        [ ( "A", 1 ), ( "B", 2 ), ( "C", 3 ) ]

    reOrder titleList annotatedList
    --> Ok [ ( "A", 1 ), ( "C", 3 ), ( "B", 2 ) ]

-}
reOrder : List String -> List ( String, a ) -> Result DocumentError (List ( String, a ))
reOrder titleList annotatedList =
    case
        ( elementsAreUnique titleList
        , List.length titleList == List.length annotatedList
        , List.sort titleList == List.sort (List.map Tuple.first annotatedList)
        )
    of
        ( False, _, _ ) ->
            Err RepeatedListElements

        ( _, False, _ ) ->
            Err ListsOfDifferentLengths

        ( _, _, False ) ->
            Err UnequalSets

        ( True, True, True ) ->
            let
                order : String -> Int
                order str =
                    List.Extra.elemIndex str titleList |> Maybe.withDefault -1
            in
            Ok (List.sortBy (\( str, _ ) -> order str) annotatedList)


{-| Reorder children in master given given (1) the list of
current child document titles, (2) a different ordering of
the titles.

Assumption: the two lists do not refer at all to master

Safety: p

-}
reorderChildrenInMaster : Document -> List String -> List String -> Result DocumentError Document
reorderChildrenInMaster masterDocument childTitleList newChildTitleList =
    let
        annotatedList =
            List.map2 (\u v -> ( u, v )) childTitleList masterDocument.childInfo
    in
    case reOrder newChildTitleList annotatedList |> Result.map (List.map Tuple.second) of
        Err err ->
            Err err

        Ok newChildInfo ->
            case equalUuidSets newChildInfo masterDocument.childInfo of
                True ->
                    Ok { masterDocument | childInfo = newChildInfo }

                False ->
                    Err UuidListsDoNotMatch


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
            String.left 4 endOfHash
    in
    document.authorIdentifier ++ "." ++ Utility.String.compress document.title ++ "." ++ shortHash


makeSlugWithTitle : Document -> String -> String
makeSlugWithTitle document title =
    let
        endOfHash =
            Uuid.toString document.id |> String.right 6

        shortHash =
            String.left 4 endOfHash
    in
    document.authorIdentifier ++ "." ++ Utility.String.compress title ++ "." ++ shortHash


makeInitialSlug : String -> String -> Uuid -> String
makeInitialSlug title authorIdentifier identifier =
    let
        endOfHash =
            Uuid.toString identifier |> String.right 6

        shortHash =
            String.left 4 endOfHash
    in
    authorIdentifier ++ "." ++ Utility.String.compress title ++ "." ++ shortHash


{-| -- create "jxxcarlson" "Intro to Chromaticity" t "First draft ..."
-- --> { authorIdentifier = "jxxcarlson", children = [], content = "First draft ..."
-- , id = "jxxcarlson.intro-to-chromaticity.1568667528"
-- , public = False, tags = [], timeCreated = Posix 1568667528000
-- , timeUpdated = Posix 1568667528000, title = "Intro to Chromaticity" }
-- : Document
-}
create : DocType -> Uuid -> String -> String -> String -> Document
create docType documentUuid authorIdentifier title content =
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
    , docType = docType
    , childInfo = []
    , permissions = []
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


{-|

    testDoc : Document
    testDoc = {dummy | content = "\\section{Intro}" }

    updateMetaData testDoc |> .title
    --> "Intro"

-}
updateMetaData : Document -> Document
updateMetaData document =
    let
        title_ =
            newTitle document
    in
    { document | slug = makeSlugWithTitle document title_, title = title_ }


newTitle : Document -> String
newTitle document =
    case getHeading document of
        Nothing ->
            document.title

        Just newTitle_ ->
            newTitle_



-- PARSING
-- PARSE LEVEL
-- PARSE HEADING


getHeading : Document -> Maybe String
getHeading document =
    DocParser.parseHeading document.content


level : String -> Int
level str =
    case DocParser.getLeadingSpace str of
        Just s ->
            String.length s // 3

        Nothing ->
            0


totalWordCount : List Document -> Int
totalWordCount documentList =
    documentList
        |> List.map (.content >> String.words >> List.length)
        |> List.sum
