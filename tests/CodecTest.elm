module CodecTest exposing (suite)

import Codec
import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Prng.Uuid as Uuid exposing (Uuid)
import Test exposing (..)
import Utility


suite : Test
suite =
    describe "Toc operations"
        [ doTest "Decode string to (UUid, Int)r" computedPair expectedPair
        ]



-- CONVENIENCE FUNCTION FOR TESTING --


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue



-- TEST DATA --


computedPair =
    Codec.getPair "(0966d323-db7d-4e61-b205-7f14db52eaf0,0)"


expectedPair =
    case Uuid.fromString "0966d323-db7d-4e61-b205-7f14db52eaf0" of
        Just uuid ->
            Just ( uuid, 0 )

        Nothing ->
            Nothing
