module TocTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Toc
import Tree exposing (Tree, singleton, tree)


ff tree =
    List.length (Tree.children tree)


updateff : Tree ( Int, Int ) -> Tree ( Int, Int )
updateff tree =
    let
        ( a, _ ) =
            Tree.label tree

        newLabel =
            ( a, ff tree )
    in
    tree |> Tree.replaceLabel newLabel


tt =
    tree ( 0, 0 ) [ tree ( 1, 0 ) [ tree ( 2, 0 ) [], tree ( 3, 0 ) [] ], tree ( 4, 0 ) [ tree ( 5, 0 ) [] ] ]


expected_tt =
    Just (tree ( 0, 2 ) [ tree ( 1, 2 ) [ tree ( 2, 0 ) [], tree ( 3, 0 ) [] ], tree ( 4, 2 ) [ tree ( 5, 0 ) [] ] ])



--test expr expectedValue comment =
--  test comment <|
--    \_ expr |> Expect.equal expectedValue


suite : Test
suite =
    describe "Toc module (Table of Contents)"
        [ test "Create document" <|
            \_ ->
                Toc.traverse Toc.updateff tt
                    |> Expect.equal (Just (Tree.singleton ( 0, 0 )))
        ]
