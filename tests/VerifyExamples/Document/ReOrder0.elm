module VerifyExamples.Document.ReOrder0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Document exposing (..)



annotatedList : List (String, Int)
annotatedList =
    [ ( "A", 1 ), ( "B", 2 ), ( "C", 3 ) ]
titleList : List String
titleList =
    [ "A", "C", "B" ]



spec0 : Test.Test
spec0 =
    Test.test "#reOrder: \n\n    reOrder titleList annotatedList\n    --> Ok [ ( \"A\", 1 ), ( \"C\", 3 ), ( \"B\", 2 ) ]" <|
        \() ->
            Expect.equal
                (
                reOrder titleList annotatedList
                )
                (
                Ok [ ( "A", 1 ), ( "C", 3 ), ( "B", 2 ) ]
                )