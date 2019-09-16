module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Document  exposing(..)
import Time



t1 = Time.millisToPosix (1568667528 * 1000)
t2 = Time.millisToPosix (1568677528 * 1000)
t3 = Time.millisToPosix (1568687528 * 1000)

d1 : Document
d1 = Document.create "jxxcarlson" "A" t1 "abc"

d1a : Document
d1a = Document.setContent "def" d1

d2 : Document
d2 = Document.create "jxxcarlson" "B" t2 "xyz"

list = [d1,d2]

suite : Test
suite =
   describe "The Document module"
               [ test "Create document" <|
                   \_ ->
                       d1.title
                           |> Expect.equal "A"

                 , test "Update document" <|
                      \_ ->
                         Document.setContent "def" d1
                           |> (\x -> x.content)
                            |> Expect.equal "def"

                  , test "Update document in list" <|
                       \_ ->
                          Document.replaceInList d1a list
                             |> Expect.equal [d1a,d2]

               ]
