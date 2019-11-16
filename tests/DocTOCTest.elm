module DocTOCTest exposing (suite)

import Dict exposing (Dict)
import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Maybe.Extra
import Prng.Uuid as Uuid exposing (Uuid)
import Result exposing (Result(..))
import Test exposing (..)
import Toc
import TocManager
import Utility exposing (getId)
import Utility.List



-- CONVENIENCE FUNCTION FOR TESTING --


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue


suite : Test
suite =
    describe "Toc operations"
        [ doTest "1. insert new document at position 1 in Master" newMaster1 expectedNewMaster1
        , doTest "2. insert new document at position 2 in Master" newMaster2 expectedNewMaster2
        , doTest "3. insert new document at position 3 in Master" newMaster3 expectedNewMaster3
        , doTest "4. insert new document at position 1 in ChildDocumentList" newChildDocumentList1 expectedNewChildDocumentList1
        , doTest "5. insert new document at position 2 in ChildDocumentList" newChildDocumentList2 expectedNewChildDocumentList2
        , doTest "6. insert new document at position 3 in ChildDocumentList" newChildDocumentList3 expectedNewChildDocumentList3
        , doTest "7. Compare computed and expected outlines " computedOutline (Just expectedOutline)
        , doTest "8. Delete document " masterAfterDelete expectedMasterAfterDelete
        , doTest "9. Document.reOrder " transformedList expectedTransformedList
        , doTest "10. Document.reorderChildren " reorderMaster (Ok expectedReorderMaster)
        , doTest "11 Update from outline - check master " newMasterXX expectedNewMasterXX
        , doTest "12. Update from outline - check document list " newDocumentListXX expectedDocumentListXX
        , doTest "Update from outline - change level, check master " newMasterFromOutline2 (Ok master3)
        , doTest "Update from bad outline - missing title" newMasterFromBadOutline (Err ListsOfDifferentLengthsTM1)
        , doTest "Update from bad outline - extra title" newMasterFromOutlineWithExtraTitle (Err ListsOfDifferentLengthsTM1)
        , doTest "Update from bad outline - extraneous title" newMasterFromOutlineWithExtraneousTitle (Err UnequalSets)
        , doTest "Identity test for reordering by titles"
            (docReorderBy [ "A", "B", "C" ] |> Result.map childTitlesFromDoc)
            (Ok [ "A", "B", "C" ])
        , doTest "Reordering by titles, list too short"
            (docReorderBy [ "A", "B" ] |> Result.map childTitlesFromDoc)
            (Err ListsOfDifferentLengths)
        , doTest "Reordering by titles, list too long"
            (docReorderBy [ "A", "B", "C", "D" ] |> Result.map childTitlesFromDoc)
            (Err ListsOfDifferentLengths)
        , doTest "Reordering by titles, list empty"
            (docReorderBy [] |> Result.map childTitlesFromDoc)
            (Err ListsOfDifferentLengths)
        , doTest "Reordering by titles, list has extraneous element"
            (docReorderBy [ "A", "X", "C" ] |> Result.map childTitlesFromDoc)
            (Err UnequalSets)
        , doTest "Permutation test for reordering by titles"
            (docReorderBy [ "B", "A", "C" ] |> Result.map childTitlesFromDoc)
            (Ok [ "B", "A", "C" ])
        , doTest "try to reorder with title list which is too short"
            (docReorderBy [ "A", "B" ] |> Result.map childTitlesFromDoc)
            (Err ListsOfDifferentLengths)
        , doTest "try to reorder with title list which is too long"
            (docReorderBy [ "A", "B", "C", "D" ] |> Result.map childTitlesFromDoc)
            (Err ListsOfDifferentLengths)
        , doTest "try to reorder with title list has extraneous element"
            (docReorderBy [ "A", "X", "C" ] |> Result.map childTitlesFromDoc)
            (Err UnequalSets)
        , doTest "equalUuid, identity"
            (Document.equalUuidSets [ ( id1, "A" ), ( id2, "A" ), ( id3, "A" ) ] [ ( id1, "A" ), ( id2, "A" ), ( id3, "A" ) ])
            True
        , doTest "equalUuid, permuted"
            (Document.equalUuidSets [ ( id2, "A" ), ( id1, "A" ), ( id3, "A" ) ] [ ( id1, "A" ), ( id2, "A" ), ( id3, "A" ) ])
            True
        , doTest "equalUuid, extraneous element"
            (Document.equalUuidSets [ ( id4, "A" ), ( id1, "A" ), ( id3, "A" ) ] [ ( id1, "A" ), ( id2, "A" ), ( id3, "A" ) ])
            False
        , doTest "equalUuid, unequal length"
            (Document.equalUuidSets [ ( id2, "A" ), ( id3, "A" ) ] [ ( id1, "A" ), ( id2, "A" ), ( id3, "A" ) ])
            False
        ]



-- DATA FOR TESTS --
-- DUMMY DOCUMENT --


dummy =
    { id = getId 1
    , title = "Text"
    , authorIdentifier = "jxxcarlson"
    , content = "empty"
    , public = False
    , tags = []
    , slug = "yada123"
    , docType = Markdown MDExtendedMath
    , childInfo = []
    , permissions = []
    }



-- IDS --


id1 =
    getId 1


id2 =
    getId 2


id3 =
    getId 3


id4 =
    getId 4


id5 =
    getId 5



-- DOCUMENTS --


da =
    { dummy | id = getId 1, title = "A" }


db =
    { dummy | id = getId 2, title = "B" }


dc =
    { dummy | id = getId 3, title = "C" }


dx =
    { dummy | id = getId 4, title = "X" }



-- ID/TITLE HELPER --


idStringTitleDict : Dict String String
idStringTitleDict =
    Dict.fromList [ ( Uuid.toString id1, "A" ), ( Uuid.toString id2, "B" ), ( Uuid.toString id3, "C" ) ]


titleIdDict : Dict String String
titleIdDict =
    let
        pairs =
            Dict.toList idStringTitleDict
    in
    List.map (\( a, b ) -> ( b, a )) pairs
        |> Dict.fromList


titleOfUuid : Uuid -> Maybe String
titleOfUuid uuid =
    Dict.get (Uuid.toString uuid) idStringTitleDict


childTitlesFromDoc : Document -> List String
childTitlesFromDoc doc =
    doc.childInfo
        |> List.map (Tuple.first >> titleOfUuid)
        |> Maybe.Extra.values



-- MASTER DOCUMMENT FOR TESTING --


master =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id3, 0 ) ] }


childDocuments =
    [ da, db, dc ]



-- TEST 1 --


newMaster1 =
    TocManager.insertInMaster dx da master


expectedNewMaster1 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id4, 1 ), ( id2, 1 ), ( id3, 0 ) ] }



-- TEST 2 --


newMaster2 =
    TocManager.insertInMaster dx db master


expectedNewMaster2 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id4, 1 ), ( id3, 0 ) ] }



-- TEST 3 --


newMaster3 =
    TocManager.insertInMaster dx dc master


expectedNewMaster3 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id3, 0 ), ( id4, 0 ) ] }



-- TEST 4 --


newChildDocumentList1 =
    TocManager.insertInChildDocumentList dx da childDocuments


expectedNewChildDocumentList1 =
    [ da, dx, db, dc ]



-- TEST 5 --


newChildDocumentList2 =
    TocManager.insertInChildDocumentList dx db childDocuments


expectedNewChildDocumentList2 =
    [ da, db, dx, dc ]



-- TEST 6 --


newChildDocumentList3 =
    TocManager.insertInChildDocumentList dx dc childDocuments


expectedNewChildDocumentList3 =
    [ da, db, dc, dx ]



-- TEST 7: Document outline --


documentList =
    [ newMaster1, da, dx, db, dc ]


expectedOutline =
    String.trim
        """
A
   X
   B
C
    """


computedOutline =
    TocManager.computeOutline newMaster1 documentList



--- TEST 8: delete document --


newMasterX =
    TocManager.insertInMaster dx da master


masterAfterDelete =
    Document.deleteChild dx newMasterX


expectedMasterAfterDelete =
    master



-- TEST 9: reOrder


titleList =
    [ "A", "C", "B" ]


annotatedList =
    [ ( "A", 1 ), ( "B", 2 ), ( "C", 3 ) ]


transformedList =
    Document.reOrder titleList annotatedList


{-| } Reorder the annotatedList using the
ordereing specified in the titleList
-}
expectedTransformedList =
    Ok [ ( "A", 1 ), ( "C", 3 ), ( "B", 2 ) ]



-- TEST 10: reorderChildren
{-
   newMaster1 = TocManager.insertInMaster dx da master
                A X B C
   newMaster2 = TocManager.insertInMaster dx db master
                A B X c

   newMaster1 ~ [ ( id1, 0 ), ( id4, 1 ), ( id2, 1 ), ( id3, 0 ) ] }
   newMaster2 ~ [ ( id1, 0 ), ( id2, 1 ), ( id4, 1 ), ( id3, 0 ) ]

-}


reorderMaster : Result DocumentError Document
reorderMaster =
    reorderChildrenInMaster newMaster1 [ "A", "X", "B", "C" ] [ "A", "B", "X", "C" ]


expectedReorderMaster =
    newMaster2



-- TEST 11, 12: update from outline


newOutline =
    String.trim
        """
A
   B
   X
C
    """


{-| ABXC
-}
newMasterXX =
    TocManager.updateMasterAndDocumentListFromOutline newOutline [ newMaster1, da, dx, db, dc ]
        |> Result.toMaybe
        |> Maybe.map Tuple.first
        |> Maybe.withDefault da


{-| ABXC
-}
expectedNewMasterXX =
    TocManager.insertInMaster dx db master


{-| ABXC
-}
newDocumentListXX =
    TocManager.updateMasterAndDocumentListFromOutline newOutline [ newMaster1, da, dx, db, dc ]
        |> Result.toMaybe
        |> Maybe.map Tuple.second
        |> Maybe.withDefault []
        |> List.drop 1
        |> List.map .title


{-| ABXC
-}
expectedDocumentListXX =
    [ da, db, dx, dc ] |> List.map .title



-- TEST 13, 14: update from outline


master2 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 0 ), ( id3, 0 ) ] }


master3 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id3, 0 ) ] }


newOutline2 =
    String.trim
        """
A
   B
C
"""


newMasterFromOutline2 =
    TocManager.updateMasterAndDocumentListFromOutline newOutline2 [ master2, da, db, dc ]
        |> Result.map Tuple.first


badOutline =
    String.trim
        """
A
B
"""


outlineWithExtraneousTitle =
    String.trim
        """
A
X
C
"""


outlineWithExtraTitle =
    String.trim
        """
A
B
C
U
"""


newMasterFromBadOutline =
    TocManager.updateMasterAndDocumentListFromOutline badOutline [ master2, da, db, dc ]
        |> Result.map Tuple.first


newMasterFromOutlineWithExtraneousTitle =
    TocManager.updateMasterAndDocumentListFromOutline outlineWithExtraneousTitle [ master2, da, db, dc ]
        |> Result.map Tuple.first


newMasterFromOutlineWithExtraTitle =
    TocManager.updateMasterAndDocumentListFromOutline outlineWithExtraTitle [ master2, da, db, dc ]
        |> Result.map Tuple.first


{-|

    > reorderedMasterABC_ABC |> Result.map childTitlesFromDoc
    Ok ["A","B","C"]

-}
reorderedMasterABC_ABC =
    Document.reorderChildrenInMaster master [ "A", "B", "C" ] [ "A", "B", "C" ]


docReorderBy =
    Document.reorderChildrenInMaster master [ "A", "B", "C" ]
