module TocZ exposing (Id, Label, TocMsg(..), darkRed, focus, inAncestors, indexedMap, viewAfter, viewBefore, viewNode, viewSelf, viewZ)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Prng.Uuid as Uuid exposing (Uuid)
import Toc exposing (TocItem)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias Id =
    Uuid


type alias Label =
    TocItem


type TocMsg
    = Focus Id
    | Toggle


focus : Id -> Zipper Label -> Zipper Label
focus id zipper =
    zipper
        |> Zipper.findFromRoot (\l -> l.id == id)
        |> Maybe.withDefault zipper


viewZ : Bool -> Zipper Label -> Element TocMsg
viewZ t z =
    List.concat
        [ viewBefore t z
        , [ viewSelf t (Zipper.tree z) ]
        , viewAfter t z
        ]
        |> column [ Font.size fontSize, spacing (spacingXAlways t) ]
        |> inAncestors t z


viewBefore : Bool -> Zipper Label -> List (Element TocMsg)
viewBefore t z =
    List.map (viewNode t) (Zipper.siblingsBeforeFocus z)


viewAfter : Bool -> Zipper Label -> List (Element TocMsg)
viewAfter t z =
    List.map (viewNode t) (Zipper.siblingsAfterFocus z)


prefix l =
    case ( l.isRoot, l.hasChildren ) of
        ( True, _ ) ->
            "   "

        ( False, Just True ) ->
            "+ "

        ( False, Just False ) ->
            "   "

        ( False, Nothing ) ->
            "   "


viewSelf : Bool -> Tree Label -> Element TocMsg
viewSelf toggle t =
    let
        l =
            Tree.label t
    in
    column []
        [ el [ Font.bold, Font.color darkRed, paddingEach { edges | bottom = 8 } ] (text <| prefix l ++ l.title)
        , column [ spacing (spacingXAlways toggle), thePadding, Font.size fontSize ] (List.map (viewNode toggle) (Tree.children t))
        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


darkRed =
    rgb 0.5 0.0 0.0


fontSize =
    12


spacingAmount =
    8


thePadding =
    paddingXY 12 0



--- paddingXY 12 0


viewNode : Bool -> Tree Label -> Element TocMsg
viewNode showAll t =
    let
        l =
            Tree.label t

        xs =
            if showAll then
                [ column [ thePadding, Font.size fontSize ] (List.map (viewNode showAll) (Tree.children t)) ]

            else
                []
    in
    column [ Font.size fontSize, spacing (spacingXIfToggleOn showAll) ]
        (Input.button []
            { onPress = Just (Focus l.id)
            , label = el [] (text <| prefix l ++ l.title)
            }
            :: xs
        )


inAncestors : Bool -> Zipper Label -> Element TocMsg -> Element TocMsg
inAncestors toggle zipper current =
    case Zipper.parent zipper of
        Just parent ->
            List.concat
                [ viewBefore toggle parent
                , [ column [ thePadding, Font.size fontSize, spacing (spacingXAlways toggle) ]
                        [ Input.button []
                            { onPress = Just (Focus (Zipper.label parent).id), label = text (Zipper.label parent).title }
                        , current
                        ]
                  ]
                , viewAfter toggle parent
                ]
                |> column [ spacing (spacingXAlways toggle) ]
                |> inAncestors toggle parent

        Nothing ->
            current


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )


spacingXNever : Bool -> Int
spacingXNever bit =
    0


spacingXAlways : Bool -> Int
spacingXAlways bit =
    spacingAmount


spacingXIfToggleOn : Bool -> Int
spacingXIfToggleOn bit =
    if bit then
        spacingAmount

    else
        0


spacingXIfToggleOff : Bool -> Int
spacingXIfToggleOff bit =
    if bit then
        0

    else
        spacingAmount
