module DocTOCTest exposing (suite)

import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Prng.Uuid as Uuid exposing (Uuid)
import Test exposing (..)
import Toc
import TocManager
import TocZ
import Utility exposing (getId)


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
        , doTest "10. Document.reorderChildren " reorderMaster expectedReorderMaster
        , doTest "11 Update from outline - check master " newMasterXX expectedNewMasterXX
        , doTest "12. Update from outline - check document list " newDocumentListXX expectedDocumentListXX
        , doTest "13. Update from outline - change level, check master " newMaster2X master3
        ]



-- CONVENIENCE FUNCTION FOR TESTING --


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue



-- DATA FOR TESTS --


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
    }


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


da =
    { dummy | id = getId 1, title = "A" }


db =
    { dummy | id = getId 2, title = "B" }


dc =
    { dummy | id = getId 3, title = "C" }


dx =
    { dummy | id = getId 4, title = "X" }


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


expectedTransformedList =
    [ ( "A", 1 ), ( "C", 3 ), ( "B", 2 ) ]



-- TEST 10: reorderChildren


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


newMaster2X =
    TocManager.updateMasterAndDocumentListFromOutline newOutline2 [ master2, da, db, dc ]
        |> Maybe.map Tuple.first
        |> Maybe.withDefault da
