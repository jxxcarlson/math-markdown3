module TocManager exposing
    ( cleanChildInfo
    , computeOutline
    , index
    , insertInChildDocumentList
    , insertInMaster
    , insertInMasterAtHead
    , setup
    , setupWithFocus
    , updateMasterAndDocumentListFromOutline
    )

import Document exposing (Document, DocumentError(..))
import List.Extra
import Prng.Uuid as Uuid exposing (Uuid)
import Toc exposing (TocItem)
import TocZ
import Tree.Zipper as Zipper exposing (Zipper)
import Utility.List


{-| Compare the childDocuments list with the childInfo
list. Delete items in the latter that are not represented
in the former.
-}
cleanChildInfo : List Document -> Document -> Document
cleanChildInfo childDocuments master =
    let
        childDocumentIds =
            List.map .id childDocuments

        updatedChildInfo =
            master.childInfo
                |> List.filter (\item -> List.member (Tuple.first item) childDocumentIds)
    in
    { master | childInfo = updatedChildInfo }



-- YYY


{-| Assume that the master is not part of the child document list
-}
setup : Maybe Document -> List Document -> Maybe (Zipper TocItem)
setup maybeMasterDocument childDocumentList =
    case maybeMasterDocument of
        Nothing ->
            Nothing

        Just masterDocument ->
            let
                sortedChildDocuments =
                    Document.sortChildren masterDocument childDocumentList

                tocData =
                    Just <| Zipper.fromTree <| Toc.make masterDocument sortedChildDocuments
            in
            tocData


{-| Assume that the master is not part of the child document list
-}
setupWithFocus : Uuid -> Maybe Document -> List Document -> Maybe (Zipper TocItem)
setupWithFocus uuid maybeMasterDocument childDocumentList =
    case maybeMasterDocument of
        Nothing ->
            Nothing

        Just masterDocument ->
            let
                sortedChildDocuments =
                    Document.sortChildren masterDocument childDocumentList

                tocData =
                    Toc.make masterDocument sortedChildDocuments
                        |> Zipper.fromTree
                        |> TocZ.focus uuid
                        |> Just
            in
            tocData


{-| Insert a new document in the list of child
documents of the master document, placing it just
after the target document.
-}
insertInMaster : Document -> Document -> Document -> Document
insertInMaster newDocument targetDocument masterDocument =
    let
        {- The plan is to insert the new document after the target index -}
        targetIndex =
            List.Extra.findIndex (\item -> Tuple.first item == targetDocument.id)
                masterDocument.childInfo
                |> Maybe.withDefault 0

        levelOfNewChild =
            case ( List.Extra.getAt targetIndex masterDocument.childInfo, List.Extra.getAt (targetIndex + 1) masterDocument.childInfo ) of
                ( Just ( _, l ), Just ( _, m ) ) ->
                    max l m

                ( Just ( _, l ), Nothing ) ->
                    l

                ( _, _ ) ->
                    0

        newItem =
            ( newDocument.id, levelOfNewChild )

        targetItem =
            ( targetDocument.id, 0 )

        eq : ( Uuid, Int ) -> ( Uuid, Int ) -> Bool
        eq ( uuid1, _ ) ( uuid2, _ ) =
            uuid1 == uuid2

        newChildInfo =
            Utility.List.insertItem eq newItem targetItem masterDocument.childInfo
    in
    { masterDocument | childInfo = newChildInfo }


{-| Insert a new document at the head of the list of child
documents of the master document, returning the new master.
-}
insertInMasterAtHead : Document -> Document -> Document
insertInMasterAtHead newDocument masterDocument =
    let
        newItem =
            ( newDocument.id, 0 )

        newChildInfo =
            newItem :: masterDocument.childInfo
    in
    { masterDocument | childInfo = newChildInfo }


equal : Document -> Document -> Bool
equal d e =
    d.id == e.id


{-| Insert a new document in a documet list immediately after the target document
-}
insertInChildDocumentList : Document -> Document -> List Document -> List Document
insertInChildDocumentList newDocument targetDocument documentList =
    Utility.List.insertItem equal newDocument targetDocument documentList


{-| Compute an indented outline (a string) from a master document
and its list of child documents. Return Nothing if the given
master document is not the document at the head of the list.

We check some necessary conditions for this to be a valid
computation before proceeding, returning Nothing if validation
fails.

-}
computeOutline : Document -> List Document -> Maybe String
computeOutline masterDocument tableOfContents =
    case
        Just masterDocument.id == Maybe.map .id (List.head tableOfContents)
    of
        False ->
            Nothing

        True ->
            let
                titles_ =
                    List.drop 1 tableOfContents |> List.map .title

                levels_ =
                    masterDocument.childInfo |> List.map Tuple.second

                ( levels, titles ) =
                    case List.length levels_ == List.length titles_ of
                        True ->
                            ( levels_, titles_ )

                        False ->
                            let
                                n =
                                    min (List.length levels_) (List.length titles_)
                            in
                            ( List.take n levels_, List.take n titles_ )
            in
            Just (List.map2 (\title level -> String.repeat (3 * level) " " ++ title) titles levels |> String.join "\n")


{-| Update the order and levels of the children of a master document based
on an outline of its child documents.

NB: We assume that the documentList is headed by the master document.
This function returns a document list headed by the updated master document

-}
updateMasterAndDocumentListFromOutline : String -> List Document -> Result DocumentError ( Document, List Document )
updateMasterAndDocumentListFromOutline documentOutline documentList =
    case List.head documentList of
        Nothing ->
            Err DocumentListIsEmpty

        Just masterDocument ->
            let
                childDocuments =
                    List.drop 1 documentList

                childTitles =
                    List.map .title childDocuments

                titleListUnfiltered =
                    String.split "\n" documentOutline

                titleList =
                    titleListUnfiltered
                        |> List.map String.trim
                        |> List.filter (\str -> str /= "")
            in
            -- Ensure that the documentList, the masterDocument.childInfo, and
            -- the title list from the document outline have the same length.
            -- If so, proceed, otherwise return an error.
            case ( List.length titleList == List.length childDocuments, List.length childDocuments == List.length masterDocument.childInfo ) of
                ( False, _ ) ->
                    Err ListsOfDifferentLengthsTM1

                ( _, False ) ->
                    Err <| ListsOfDifferentLengthsTM2 ("child docs: " ++ String.fromInt (List.length childDocuments) ++ ", master doc, children: " ++ String.fromInt (List.length masterDocument.childInfo))

                ( True, True ) ->
                    let
                        newLevels_ : List ( String, Int )
                        newLevels_ =
                            -- the levels derived from the document outline
                            List.map2 (\x y -> ( x, y ))
                                (List.map String.trim titleListUnfiltered)
                                (List.map Document.level titleListUnfiltered)

                        newLevels : List Int
                        newLevels =
                            -- Put the levels in the initial order
                            case Document.reOrder childTitles newLevels_ of
                                Ok levels_ ->
                                    List.map Tuple.second levels_

                                Err _ ->
                                    []

                        newChildInfo =
                            -- childInfo with updated levels
                            List.map2 (\( id, _ ) l -> ( id, l )) masterDocument.childInfo newLevels

                        newMasterDocument_ =
                            -- masterDocument_ with updated levels
                            { masterDocument | childInfo = newChildInfo }
                    in
                    case Document.reorderChildrenInMaster newMasterDocument_ (List.map .title childDocuments) titleList of
                        Ok newMasterDocument ->
                            let
                                newDocumentList =
                                    newMasterDocument :: (childDocuments |> Document.sortChildren newMasterDocument)
                            in
                            Ok ( newMasterDocument, newDocumentList )

                        Err error ->
                            Err error


{-| Find index of subDocument in Master
-}
index : Document -> Document -> Maybe Int
index subDocument master =
    List.Extra.findIndex (\item -> Tuple.first item == subDocument.id) master.childInfo
