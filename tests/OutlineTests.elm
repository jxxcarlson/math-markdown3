module OutlineTests exposing (da, db, dc, dd, de, master, suite)

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


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue


suite : Test
suite =
    describe "Toc operations"
        [ doTest "est initial outline"
            (TocManager.computeOutline master initialDocumentList)
            (Just initialOutline)
        , doTest "Update by outline (identity test)"
            (updateMaster initialOutline)
            (Just master)
        , doTest "Permute first two subdocs"
            (updateMaster outline2)
            (Just master2)
        , doTest "Permute first two subdocs and change level"
            (updateMaster outline3)
            (Just master3)
        ]


updateMaster outline_ =
    TocManager.updateMasterAndDocumentListFromOutline outline_ initialDocumentList |> Result.map Tuple.first |> Result.toMaybe


initialOutline =
    String.trim
        """
A
B
C
D
E
"""



-- TEST 2: Permute first two subdocs


outline2 =
    String.trim
        """
B
A
C
D
E
"""


master2 =
    { master | childInfo = [ ( idB, 0 ), ( idA, 0 ), ( idC, 0 ), ( idD, 0 ), ( idE, 0 ) ] }



-- TEST 3: Permute first two subdocs and change level


outline3 =
    String.trim
        """
B
   A
C
D
E
"""


master3 =
    { master | childInfo = [ ( idB, 0 ), ( idA, 1 ), ( idC, 0 ), ( idD, 0 ), ( idE, 0 ) ] }



-- SETUP


initialDocumentList =
    [ master, da, db, dc, dd, de ]



-- DATA FOR TESTS --


dummy =
    { id = idA
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
-- DOCUMENTS --


idA =
    getId 1


idB =
    getId 2


idC =
    getId 3


idD =
    getId 4


idE =
    getId 5


id6 =
    getId 6


master =
    { dummy | id = id6, title = "Master", childInfo = [ ( idA, 0 ), ( idB, 0 ), ( idC, 0 ), ( idD, 0 ), ( idE, 0 ) ] }


da =
    { dummy | id = idA, title = "A" }


db =
    { dummy | id = idB, title = "B" }


dc =
    { dummy | id = idC, title = "C" }


dd =
    { dummy | id = idD, title = "D" }


de =
    { dummy | id = idE, title = "E" }
