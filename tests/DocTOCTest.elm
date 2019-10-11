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


suite : Test
suite =
    describe "Toc operations"
        [ doTest "insert new document at position 1" newMaster1 expectedNewMaster1
        , doTest "insert new document at position 2" newMaster2 expectedNewMaster2
        , doTest "insert new document at position 3" newMaster3 expectedNewMaster3
        ]
