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
    }


master =
    { dummy | title = "master", children = [ getId 2, getId 3, getId 4 ], childLevels = [ 0, 1, 0 ] }


childDocuments =
    [ ch1, ch2, ch3 ]


ch1 =
    { dummy | id = getId 2, title = "A" }


ch2 =
    { dummy | id = getId 3, title = "B" }


ch3 =
    { dummy | id = getId 4, title = "C" }


dx =
    { dummy | id = getId 5, title = "X" }



-- TEST 1 --


newMaster1 =
    TocManager.insertInMaster dx ch1 master


expectedNewMaster1 =
    { dummy | title = "master", children = [ getId 2, getId 5, getId 3, getId 4 ], childLevels = [ 0, 1, 1, 0 ] }



-- TEST 2 --


newMaster2 =
    TocManager.insertInMaster dx ch2 master


expectedNewMaster2 =
    { dummy | title = "master", children = [ getId 2, getId 3, getId 5, getId 4 ], childLevels = [ 0, 1, 1, 0 ] }



-- TEST 3 --


newMaster3 =
    TocManager.insertInMaster dx ch3 master


expectedNewMaster3 =
    { dummy | title = "master", children = [ getId 2, getId 3, getId 4, getId 5 ], childLevels = [ 0, 1, 0, 0 ] }



-- TEST 4 --


equal : ( Int, Int ) -> ( Int, Int ) -> Bool
equal x y =
    Tuple.first x == Tuple.first y


primes =
    [ ( 1, 2 ), ( 2, 3 ), ( 3, 5 ) ]


newPrimes1 =
    Utility.insertItemInList equal ( 4, 7 ) ( 1, 2 ) primes


expectedNewPrimes1 =
    [ ( 1, 2 ), ( 4, 7 ), ( 2, 3 ), ( 3, 5 ) ]



-- TEST 5 --


newPrimes2 =
    Utility.insertItemInList equal ( 4, 7 ) ( 2, 3 ) primes


expectedNewPrimes2 =
    [ ( 1, 2 ), ( 2, 3 ), ( 4, 7 ), ( 3, 5 ) ]



-- TEST 6 --


newPrimes3 =
    Utility.insertItemInList equal ( 4, 7 ) ( 3, 5 ) primes


expectedNewPrimes3 =
    [ ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 4, 7 ) ]



-- TEST 7 --


newChildDocumentList1 =
    TocManager.insertInChildDocumentList dx ch1 childDocuments


expectedNewChildDocumentList1 =
    [ ch1, dx, ch2, ch3 ]



-- TEST 8 --


newChildDocumentList2 =
    TocManager.insertInChildDocumentList dx ch2 childDocuments


expectedNewChildDocumentList2 =
    [ ch1, ch2, dx, ch3 ]



-- TEST 9 --


newChildDocumentList3 =
    TocManager.insertInChildDocumentList dx ch3 childDocuments


expectedNewChildDocumentList3 =
    [ ch1, ch2, ch3, dx ]


bozo =
    [ dx, ch1, ch2, ch3 ]


suite : Test
suite =
    describe "Toc operations"
        [ doTest "1. insert new document at position 1 in Master" newMaster1 expectedNewMaster1
        , doTest "2. insert new document at position 2 in Master" newMaster2 expectedNewMaster2
        , doTest "3. insert new document at position 3 in Master" newMaster3 expectedNewMaster3
        , doTest "4. insert new pair at position 1 in primes" newPrimes1 expectedNewPrimes1
        , doTest "5. insert new pair at position 2 in primes" newPrimes2 expectedNewPrimes2
        , doTest "6. insert new pair at position 3 in primes" newPrimes3 expectedNewPrimes3
        , doTest "7. insert new document at position 1 in ChildDocumentList" newChildDocumentList1 expectedNewChildDocumentList1
        , doTest "8. insert new document at position 2 in ChildDocumentList" newChildDocumentList2 expectedNewChildDocumentList2
        , doTest "9. insert new document at position 3 in ChildDocumentList" newChildDocumentList3 expectedNewChildDocumentList3
        ]
