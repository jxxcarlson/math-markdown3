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
    , children = []
    , childLevels = []
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


ch1 =
    { dummy | id = getId 1, title = "A" }


ch2 =
    { dummy | id = getId 2, title = "B" }


ch3 =
    { dummy | id = getId 3, title = "C" }


dx =
    { dummy | id = getId 4, title = "X" }


master =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id3, 0 ) ] }


childDocuments =
    [ ch1, ch2, ch3 ]



-- TEST 1 --


newMaster1 =
    TocManager.insertInMaster dx ch1 master


expectedNewMaster1 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id4, 1 ), ( id2, 1 ), ( id3, 0 ) ] }



-- TEST 2 --


newMaster2 =
    TocManager.insertInMaster dx ch2 master


expectedNewMaster2 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id4, 1 ), ( id3, 0 ) ] }



-- TEST 3 --


newMaster3 =
    TocManager.insertInMaster dx ch3 master


expectedNewMaster3 =
    { dummy | title = "master", childInfo = [ ( id1, 0 ), ( id2, 1 ), ( id3, 0 ), ( id4, 0 ) ] }



-- TEST 4 --


newChildDocumentList1 =
    TocManager.insertInChildDocumentList dx ch1 childDocuments


expectedNewChildDocumentList1 =
    [ ch1, dx, ch2, ch3 ]



-- TEST 5 --


newChildDocumentList2 =
    TocManager.insertInChildDocumentList dx ch2 childDocuments


expectedNewChildDocumentList2 =
    [ ch1, ch2, dx, ch3 ]



-- TEST 6 --


newChildDocumentList3 =
    TocManager.insertInChildDocumentList dx ch3 childDocuments


expectedNewChildDocumentList3 =
    [ ch1, ch2, ch3, dx ]



-- TEST 7: Document outline --


documentList =
    [ newMaster1, d1, d4, d2, d3 ]


d1 =
    { dummy | id = id1, title = "A" }


d2 =
    { dummy | id = id1, title = "B" }


d3 =
    { dummy | id = id1, title = "C" }


d4 =
    { dummy | id = id1, title = "X" }


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
    TocManager.insertInMaster dx ch1 master


masterAfterDelete =
    Document.deleteChild dx newMasterX


expectedMasterAfterDelete =
    master
