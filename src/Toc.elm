module Toc exposing (TocItem, make, render, setLabel, setLabels, setVisibility, traverse, traverse_)

{-| Note: for tests, run

    Ilias' code: https://ellie-app.com/6SZgmVLt8BSa1

    elm - test / tests / TocTest.elm

I.

The idea is the following. Consider a table of contents, simply as
a piece of text. Each level of the table is indented, as in this example:

A.

Introduction
States of Matter
Gases
Liquids
Solid
Energy
Kinetic
Potential
Conclusion

This defines the rose tree

B.

                              Master Document
                                    |
      ------------------------------------------------------------------
      |              |                           |                     |
    Intro     States of Matter                  Energy             Conclusion
                     |                            |
            --------------------             --------------
            |         |         |            |            |
         Gases      Liquids    Solids     Kinetic      Potential

The critical piece of information needed to transform a list of strings
such as that represented by the rows of A into a rosetree is a notion of level,
represented in A by indentation, e.g. the integer quotient by 3 of the number
of leading spaces. With this in mind, an equivalent structure is the list
of tuples

[("Introduction", 0), ("States of Matter", 0), ("Gases", 1), ("Liquids", 1) ... ]

In this case the function

    level: : (String, Int) -> Int
    level = Tuple.second

does the job.

II. Now consider the problem of presenting a collapsed list which looks lie this:

Introduction

  - States of Matter
  - Energy
    Conclusion

where the entries are titles of documents. These documents are defined
in another document, a so-called master. They are defined by
a list of the Uuid's of the "child" documents. Also part the
structure of a document is a list of integers which give the levels.
Those two lists have to be kept in sync by operating on them
simultaneously (I have done a poor job of this).

When one clicks on "States of Matter", the list should open up:

Introduction

  - States of Matter
    Gases
    Liquids
    Solid
  - Energy
    Conclusion

In general there will be deeper levels to expose. In any case, the
right way to model all of this is as a rose tree there the labels
on the nodes are records containing various document metadata
needed to manage the Table of contents, e.g., title, id, level,
visibility, etc. One starts off only the root and its children
marked with vislibiity : True. To "open" a new layer is to
mark the children of a subtree as visible (or maybe the
entire subtree, depending on the desired effect.

III. Implementation

To make this work in the context of an application, one needs to

1.  Create a rose tree from the data in the master document and
    also the corresponding list of of child documents (since
    the metadata in the master document contains uuids, but not,
    for example titles).

2.  Store the rosetree in the model

3.  Store a Maybe currentLabel

4.  Flatten the tree for display

5.  Modify the tree in response to user actions, e.g. clicking on
    the title of a document in the displayed table of contents.
    The items in the displayed table of contents are in fact
    buttons which can send a message:

    onPress = Just (SetCurrentSubDocument document tocItem)

    The message has the information needed to modify the tree
    stored as model.tocTree and if necessary modify
    model.currentTocLabel

Step (1) is carried out by the function `Toc.make`, which calls
upon `HTree.fromList` which builds a rose tree from an
annotated list -- a list endowed with a level function
on its elements. That function is defined in the library
jxxcarlson/htree. Next, `traverse updateChildren` is run
on the rosetree to add some needed info to the labels of the
tree. The homebrew `traverse` function below has type

    traverse : (Tree a -> Tree a) -> Tree a -> Maybe (Tree a)

With a better implementation, this might be a possible addition
to zwilias/elm-rosetree. The traverse function is like
mapLabels, except that function used for mapping depends
not just on the label, but on the entire subtree corresponding
to a given label.

The `traverse` function calls upon

     traverse_ : (Tree a -> Tree a) -> Maybe (Zipper a) -> Maybe (Zipper a)

which is defined using recursion (Dangerous?)

Step (5) is carried out in the context of the application by
the function

    setVisibility : Bool -> Uuid -> Tree TocItem -> Tree TocItem

It, in turn, calls on the more general

    setLabels : i -> (a -> i) -> (a -> a) -> Tree a -> Tree a
    setLabels givenIdentifier getIdentifier modifyLabel tree = ...

This function assumes that the labels of the trees
have a unique identifier. It finds the node whose label
has the given identifier, then modifies that label and the
labels of its immediate descendants using the function mapLabel.

NOTES.

1.  There is a weaker function `setLabel` with the same
    type signature as `setLabels` It changes only the
    label of the node that it finds.

2.  There should probably be a stronger function, again with te
    the same signature, that changes labels on the full subtree
    headed by the given node.

3.  On further thought, perhaps the best solution is an additional
    argument

```
    depth : Maybe Int

If it is Nothing, the full subtree is modified. If it is Just k,
the subtree is modified up through depth k. Just 0 and Just 1
would handle the cases already handled. No need to have so
many functions.
```

-}

import Document exposing (Document)
import HTree
import Prng.Uuid as Uuid exposing (Uuid)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-| The type for the labels on the trees we use
for managing tables of content
-}
type alias TocItem =
    { id : Uuid
    , title : String
    , level : Int
    , visible : Bool
    , hasChildren : Maybe Bool
    , isRoot : Bool
    }


{-| The root element for the Table of Contents
-}
rootElement : Document -> TocItem
rootElement document =
    { id = document.id
    , title = document.title
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


{-| Update the TocItem which is the label for the root of tree.
Note that while this function modifies the label, it depends on the tree
-}
updateHasChildren : Tree TocItem -> Tree TocItem
updateHasChildren tree =
    let
        label_ =
            Tree.label tree
    in
    tree |> Tree.replaceLabel { label_ | hasChildren = hasChildren_ tree }


{-| Set the visibility to 'bit' for the node correspponding to uuid.
Do the same for its immediate descendents.
-}
setVisibility : Bool -> Uuid -> Tree TocItem -> Tree TocItem
setVisibility bit uuid tree =
    setLabels uuid (\label -> label.id) (\label -> { label | visible = bit }) tree



-- GENERIC FUNCTIONS --


{-|

     traverse f tree maps one tree to another
     using a function f : Tree a -> Tree a

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


    tt = tree (0,0) [tree (1,0) [tree (2,0) [], tree (3,0) [] ], tree (4,0) [tree (5,0) []]]
         Tree (0,0) [Tree (1,0) [Tree (2,0) [],Tree (3,0) []],Tree (4,0) [Tree (5,0) []]]
         : Tree ( number, number1 )

    traverse Toc.updateff tt
      Just (Tree (0,2) [Tree (1,2) [Tree (2,0) [],Tree (3,0) []],Tree (4,1) [Tree (5,0) []]])
         : Maybe (Tree ( Int, Int ))

-}
traverse : (Tree a -> Tree a) -> Tree a -> Maybe (Tree a)
traverse f tree =
    let
        maybeZipper : Maybe (Zipper a)
        maybeZipper =
            Maybe.map (Zipper.mapTree f) (Just (Zipper.fromTree tree))
    in
    Maybe.map Zipper.toTree (traverse_ f maybeZipper)


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


{-| Find the node with the givenIdentifier. Then apply
modifyLabel to that node.
-}
setLabel : i -> (a -> i) -> (a -> a) -> Tree a -> Tree a
setLabel givenIdentifier getIdentifier modifyLabel tree =
    let
        focusedZipper : Maybe (Zipper a)
        focusedZipper =
            Zipper.findFromRoot (\label -> getIdentifier label == givenIdentifier) (Zipper.fromTree tree)

        subTreeAtFocus : Maybe (Tree a)
        subTreeAtFocus =
            Maybe.map Zipper.tree focusedZipper

        newSubTreeAtFocus : Maybe (Tree a)
        newSubTreeAtFocus =
            Maybe.map (Tree.mapLabel (\label -> modifyLabel label)) subTreeAtFocus
    in
    case Maybe.map2 Zipper.replaceTree newSubTreeAtFocus focusedZipper of
        Nothing ->
            tree

        Just newZipper ->
            Zipper.toTree newZipper


{-| Find the node with the givenIdentifier. Then apply
modifyLabel to it and to its immediate descendants.
-}
setLabels : i -> (a -> i) -> (a -> a) -> Tree a -> Tree a
setLabels givenIdentifier getIdentifier modifyLabel tree =
    let
        focusedZipper : Maybe (Zipper a)
        focusedZipper =
            Zipper.findFromRoot (\label -> getIdentifier label == givenIdentifier) (Zipper.fromTree tree)

        subTreeLabel : Maybe a
        subTreeLabel =
            Maybe.map Zipper.label focusedZipper |> Maybe.map (\label -> modifyLabel label)

        mapListTree : List (Tree a) -> List (Tree a)
        mapListTree list =
            List.map (Tree.mapLabel (\label -> modifyLabel label)) list

        childrenAtFocus : Maybe (List (Tree a))
        childrenAtFocus =
            Maybe.map Zipper.children focusedZipper
                |> Maybe.map mapListTree

        newSubTree : Maybe (Tree a)
        newSubTree =
            Maybe.map2 Tree.tree subTreeLabel childrenAtFocus
    in
    case Maybe.map2 Zipper.replaceTree newSubTree focusedZipper of
        Nothing ->
            tree

        Just newZipper ->
            Zipper.toTree newZipper


render : Tree a -> List a
render tree =
    Tree.flatten tree
