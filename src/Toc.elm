module Toc exposing (TocItem, ff, make, render, setVisibility, traverse, traverse_, updateff)

import Document exposing (Document)
import HTree
import Prng.Uuid as Uuid exposing (Uuid)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Utility


type alias TocItem =
    { id : Uuid
    , title : String
    , level : Int
    , visible : Bool
    , hasChildren : Maybe Bool
    , isRoot : Bool
    }


rootElement : Document -> TocItem
rootElement document =
    { id = document.id
    , title = document.title
    , level = -1
    , visible = True
    , hasChildren = Just True
    , isRoot = True
    }


fake : TocItem
fake =
    { id = Utility.getId 0
    , title = "foo"
    , level = -1
    , visible = True
    , hasChildren = Just True
    , isRoot = True
    }


level : TocItem -> Int
level tocItem =
    tocItem.level


prepare : Document -> List Document -> List TocItem
prepare master childDocuments =
    let
        idList =
            master.children

        levels =
            master.childLevels

        titles =
            List.map .title childDocuments

        initialVisibility : Int -> Bool
        initialVisibility k =
            if k == 0 then
                True

            else
                False

        visibles =
            List.map initialVisibility levels
    in
    List.map4 (\i t l v -> TocItem i t l v (Just False) False)
        idList
        titles
        levels
        visibles


make : Document -> List Document -> Tree TocItem
make master childDocuments =
    HTree.fromList (rootElement master) level (prepare master childDocuments)
        |> traverse updateHasChildren
        |> Maybe.withDefault (Tree.singleton (rootElement master))


{-| Does the root of the tree have children?
-}
hasChildren_ : Tree TocItem -> Maybe Bool
hasChildren_ tree =
    Just (Tree.children tree /= [])


traverse : (Tree a -> Tree a) -> Tree a -> Maybe (Tree a)
traverse f tree =
    let
        maybeZipper : Maybe (Zipper a)
        maybeZipper =
            Maybe.map (Zipper.mapTree f) (Just (Zipper.fromTree tree))
    in
    Maybe.map Zipper.toTree (traverse_ f maybeZipper)



-- Maybe.map Zipper.toTree maybeZipper


traverse_ : (Tree a -> Tree a) -> Maybe (Zipper a) -> Maybe (Zipper a)
traverse_ f maybeZipper =
    case Maybe.map Zipper.forward maybeZipper of
        Nothing ->
            maybeZipper

        Just anotherZipper ->
            case traverse_ f <| Maybe.map (Zipper.mapTree f) anotherZipper of
                Nothing ->
                    anotherZipper

                Just z ->
                    Just z


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


{-| Update the TocItem which is the label for the root of tree.
Note that while this function modifies the label, it depends on the tree
-}
updateHasChildren : Tree TocItem -> Tree TocItem
updateHasChildren tree =
    let
        la =
            Tree.label tree
    in
    tree |> Tree.replaceLabel { la | hasChildren = hasChildren_ tree }


{-| Toggle the visibility of the children of the
node with the given uuid
-}
setVisibility : Bool -> Uuid -> Tree TocItem -> Tree TocItem
setVisibility bit uuid tree =
    let
        focusedZipper : Maybe (Zipper TocItem)
        focusedZipper =
            Zipper.findFromRoot (\node -> node.id == uuid) (Zipper.fromTree tree)

        subTreeLabel : Maybe TocItem
        subTreeLabel =
            Maybe.map Zipper.label focusedZipper

        mapListTree : List (Tree TocItem) -> List (Tree TocItem)
        mapListTree list =
            List.map (Tree.mapLabel (\label -> { label | visible = bit })) list

        childrenAtFocus : Maybe (List (Tree TocItem))
        childrenAtFocus =
            Maybe.map Zipper.children focusedZipper
                |> Maybe.map mapListTree

        newSubTree : Maybe (Tree TocItem)
        newSubTree =
            Maybe.map2 Tree.tree subTreeLabel childrenAtFocus
    in
    case Maybe.map2 Zipper.replaceTree newSubTree focusedZipper of
        Nothing ->
            tree

        Just newZipper ->
            Zipper.toTree newZipper


render : Tree TocItem -> List TocItem
render tree =
    Tree.flatten tree
