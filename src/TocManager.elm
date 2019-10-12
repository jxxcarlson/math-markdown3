module TocManager exposing
    (  computeOutline
       -- , deleteDocumentInMaster

    , insertInChildDocumentList
    , insertInMaster
    , setup
    , updateChildren
    )

import Document exposing (Document)
import List.Extra
import Prng.Uuid as Uuid exposing (Uuid)
import Toc exposing (TocItem)
import TocZ
import Tree.Zipper as Zipper exposing (Zipper)
import Utility


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

                tocCursor =
                    Just masterDocument.id
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
            Debug.log "TI"
                (List.Extra.findIndex (\item -> Tuple.first item == targetDocument.id)
                    masterDocument.childInfo
                    |> Maybe.withDefault 0
                )

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
            Utility.insertItemInList eq newItem targetItem masterDocument.childInfo
    in
    { masterDocument | childInfo = newChildInfo }


equal : Document -> Document -> Bool
equal d e =
    d.id == e.id


{-| Insert a new document in a documet list immediately after the target document
-}
insertInChildDocumentList : Document -> Document -> List Document -> List Document
insertInChildDocumentList newDocument targetDocument documentList =
    Utility.insertItemInList equal newDocument targetDocument documentList



--deleteDocumentInMaster : Document -> Document -> Document
--deleteDocumentInMaster subDocument masterDocument =
--    let
--        indexOfChildToDelete =
--            List.Extra.findIndex (\item -> Tuple.first item == subDocument.id) masterDocument.childInfo
--
--        newMasterDocument_ =
--            Document.deleteChild subDocument masterDocument
--
--        newChildInfo =
--            case indexOfChildToDelete of
--                Nothing ->
--                    masterDocument.childInfo
--
--                Just idx ->
--                    List.Extra.removeAt idx masterDocument.childInfo
--    in
--    { newMasterDocument_ | childInfo = newChildInfo }
--


{-| Compte an indented outline (a string) from a master document
and its list of child documents. Return Nothing if the given
master document is not the document at the head of the list.

We check some necessary conditions for this to be a valid
computation before proceeding, returning Nothing if validation
fails.

-}
computeOutline : Document -> List Document -> Maybe String
computeOutline masterDocument childDocumentList =
    case
        Just masterDocument.id == Maybe.map .id (List.head childDocumentList)
    of
        False ->
            Nothing

        True ->
            let
                titles_ =
                    List.drop 1 childDocumentList |> List.map .title

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

-}
updateChildren : String -> List Document -> Maybe ( Document, List Document )
updateChildren documentOutline documentList =
    case List.head documentList of
        Nothing ->
            Nothing

        Just masterDocument ->
            let
                childDocuments =
                    List.drop 1 documentList

                titleList =
                    String.split "\n" documentOutline
                        |> List.map String.trim
                        |> List.filter (\str -> str /= "")

                newMasterDocument_ =
                    Document.reorderChildren masterDocument titleList childDocuments

                newMasterDocument =
                    Document.setLevelsOfChildren documentOutline newMasterDocument_

                newDocumentList =
                    newMasterDocument
                        :: childDocuments
                        |> Document.sortChildren newMasterDocument
            in
            Just ( newMasterDocument, newDocumentList )
