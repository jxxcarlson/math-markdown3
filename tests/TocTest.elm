module TocTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Toc
import Tree exposing (Tree, singleton, tree)



-- CONVENIENCE FUNCTION FOR TESTING --


doTest : String -> a -> a -> Test
doTest comment expr expectedValue =
    test comment <|
        \_ -> expr |> Expect.equal expectedValue



-- DATA FOR TESTS --


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


ttExpected =
    Just (tree ( 0, 2 ) [ tree ( 1, 2 ) [ tree ( 2, 0 ) [], tree ( 3, 0 ) [] ], tree ( 4, 1 ) [ tree ( 5, 0 ) [] ] ])


gg : Tree a -> Bool
gg tree =
    List.length (Tree.children tree) > 0


updategg : Tree ( Int, Bool ) -> Tree ( Int, Bool )
updategg tree =
    let
        ( a, _ ) =
            Tree.label tree

        newLabel =
            ( a, gg tree )
    in
    tree |> Tree.replaceLabel newLabel


ss =
    tree ( 0, False )
        [ tree ( 1, False )
            [ tree ( 2, False ) []
            , tree ( 3, False ) []
            ]
        , tree ( 4, False ) [ tree ( 5, False ) [] ]
        ]


ssExpected2 =
    Just <|
        tree ( 0, True )
            [ tree ( 1, True )
                [ tree ( 2, False ) []
                , tree ( 3, False ) []
                ]
            , tree ( 4, True ) [ tree ( 5, False ) [] ]
            ]


ssExpected3 =
    tree ( 0, False )
        [ tree ( 1, True )
            [ tree ( 2, True ) []
            , tree ( 3, True ) []
            ]
        , tree ( 4, False ) [ tree ( 5, False ) [] ]
        ]


ssExpected4 =
    tree ( 0, False )
        [ tree ( 1, True )
            [ tree ( 2, False ) []
            , tree ( 3, False ) []
            ]
        , tree ( 4, False ) [ tree ( 5, False ) [] ]
        ]


uu =
    tree ( 0, False )
        [ tree ( 1, False )
            [ tree ( 2, False )
                [ tree ( 7, False ) []
                ]
            , tree ( 3, False ) []
            ]
        , tree ( 4, False ) [ tree ( 5, False ) [] ]
        ]


uuExpected5 =
    tree ( 0, False )
        [ tree ( 1, True )
            [ tree ( 2, True )
                [ tree ( 7, False ) []
                ]
            , tree ( 3, True ) []
            ]
        , tree ( 4, False ) [ tree ( 5, False ) [] ]
        ]


suite : Test
suite =
    describe "Toc module (Table of Contents)"
        [ doTest "1. run traverse" (Toc.traverse updateff tt) ttExpected
        , doTest "2. run traverse with different function" (Toc.traverse updategg ss) ssExpected2
        , doTest "3. run setLabels" (Toc.setLabels 1 (\( i, b ) -> i) (\( i, b ) -> ( i, True )) ss) ssExpected3
        , doTest "4. run setLabel" (Toc.setLabel 1 (\( i, b ) -> i) (\( i, b ) -> ( i, True )) ss) ssExpected4
        , doTest "5. run setLabels" (Toc.setLabels 1 (\( i, b ) -> i) (\( i, b ) -> ( i, True )) uu) uuExpected5
        ]



--setLabels : i -> (a -> i) -> (a -> a) -> Tree a -> Tree a
--setLabels givenIdentifier getIdentifier modifyLabel tree
---- setLabels uuid (\label -> label.id) (\label -> { label | visible = bit }) tree
