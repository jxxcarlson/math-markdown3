module Toc exposing (TocItem, make, render, setVisibility)

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
    }


rootElement : Document -> TocItem
rootElement document =
    { id = document.id
    , title = document.title
    , level = -1
    , visible = True
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
    List.map4 TocItem idList titles levels visibles


make : Document -> List Document -> Tree TocItem
make master childDocuments =
    HTree.fromList (rootElement master) level (prepare master childDocuments)


{-| Toggle the visibility of the chilren of the
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
