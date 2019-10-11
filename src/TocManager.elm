module TocManager exposing (insertInChildDocumentList, insertInMaster)

import Document exposing (Document)
import List.Extra
import TocZ
import Utility


insertInMaster : Document -> Document -> Document -> Document
insertInMaster newDocument targetDocument masterDocument =
    let
        {- The plan is to insert the new document after the target index -}
        targetIndex =
            List.Extra.elemIndex targetDocument.id masterDocument.children |> Maybe.withDefault 0

        levelOfNewChild =
            case ( List.Extra.getAt targetIndex masterDocument.childLevels, List.Extra.getAt (targetIndex + 1) masterDocument.childLevels ) of
                ( Just l, Just m ) ->
                    max l m

                ( Just l, Nothing ) ->
                    l

                ( _, _ ) ->
                    0

        newChildren =
            Utility.insertUuidInList newDocument.id targetDocument.id masterDocument.children

        newLevels =
            Utility.insertIntegerAtIndex levelOfNewChild targetIndex masterDocument.childLevels
                |> List.take (List.length newChildren)

        -- ensure that the lists have the same length
    in
    { masterDocument | children = newChildren, childLevels = newLevels }


equal : Document -> Document -> Bool
equal d e =
    d.id == e.id


insertInChildDocumentList : Document -> Document -> List Document -> List Document
insertInChildDocumentList newDocument targetDocument childDocuments =
    Utility.insertItemInList equal newDocument targetDocument childDocuments



--    Document.insertDocumentInList newDocument targetDocument childDocuments
--        |> Document.replaceInList masterDocument
--        |> Document.sortChildren masterDocument
