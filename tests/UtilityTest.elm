module UtilityTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Prng.Uuid as Uuid exposing (Uuid)
import Test exposing (..)
import Utility exposing (getId)



-- CONVENIENCE FUNCTION FOR TESTING --


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue



-- DATA FOR TESTS --


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



-- TEST 1 --


equal : ( Int, Int ) -> ( Int, Int ) -> Bool
equal x y =
    Tuple.first x == Tuple.first y


primes =
    [ ( 1, 2 ), ( 2, 3 ), ( 3, 5 ) ]


newPrimes1 =
    Utility.insertItemInList equal ( 4, 7 ) ( 1, 2 ) primes


expectedNewPrimes1 =
    [ ( 1, 2 ), ( 4, 7 ), ( 2, 3 ), ( 3, 5 ) ]



-- TEST 2 --


newPrimes2 =
    Utility.insertItemInList equal ( 4, 7 ) ( 2, 3 ) primes


expectedNewPrimes2 =
    [ ( 1, 2 ), ( 2, 3 ), ( 4, 7 ), ( 3, 5 ) ]



-- TEST 3 --


newPrimes3 =
    Utility.insertItemInList equal ( 4, 7 ) ( 3, 5 ) primes


expectedNewPrimes3 =
    [ ( 1, 2 ), ( 2, 3 ), ( 3, 5 ), ( 4, 7 ) ]



-- TEST 4 --


data : List ( Uuid, Int )
data =
    [ ( id1, 11 ), ( id2, 22 ), ( id3, 33 ) ]


datum : ( Uuid, Int )
datum =
    ( id4, 44 )


eq : ( Uuid, Int ) -> ( Uuid, Int ) -> Bool
eq ( uuid1, _ ) ( uuid2, _ ) =
    uuid1 == uuid2


newData1 =
    Utility.insertItemInList eq datum ( id1, 11 ) data


expectedData1 =
    [ ( id1, 11 ), datum, ( id2, 22 ), ( id3, 33 ) ]


suite : Test
suite =
    describe "Toc operations"
        [ doTest "1. insert new pair at position 1 in primes" newPrimes1 expectedNewPrimes1
        , doTest "2 insert new pair at position 2 in primes" newPrimes2 expectedNewPrimes2
        , doTest "3. insert new pair at position 3 in primes" newPrimes3 expectedNewPrimes3
        , doTest "4. insert new datum at position 1 in data" newData1 expectedData1
        ]
