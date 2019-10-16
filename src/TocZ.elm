module TocZ exposing
    ( Id
    , Label
    , TocMsg(..)
    , darkRed
    , focus
    , inAncestors
    , indexedMap
    , viewAfter
    , viewBefore
    , viewNode
    , viewSelf
    , viewZ
    )

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
    let
        offset =
            if (Zipper.label z).isRoot then
                0

            else
                12
    in
    (List.concat
        [ viewBefore t z
        , [ viewSelf t (Zipper.tree z) ]
        , viewAfter t z
        ]
        |> column []
        |> inAncestors t z
    )
        |> Element.el [ moveRight offset ]


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

        color =
            if l.isRoot then
                darkBlue

            else
                darkRed
    in
    column []
        [ el [ paddingEach { edges | bottom = 2, top = 2 }, Font.bold, Font.color color ] (text <| prefix l ++ l.title)
        , column [ horizontalPadding ] (List.map (viewNode toggle) (Tree.children t))
        ]


elementHeight =
    px 20


focusedElementHeight =
    px 22


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


darkRed =
    rgb 0.5 0.0 0.0


darkBlue =
    rgb 0.0 0.0 0.65


fontSize =
    12


buttonPadding =
    paddingXY 0 4


thePadding =
    paddingXY 12 0


viewNode : Bool -> Tree Label -> Element TocMsg
viewNode showAll t =
    let
        l =
            Tree.label t

        xs =
            if showAll then
                [ column [ horizontalPadding ] (List.map (viewNode showAll) (Tree.children t)) ]

            else
                []
    in
    column []
        (Input.button [ buttonPadding, height elementHeight ]
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
                , [ column []
                        [ Input.button [ buttonPadding, Font.color (buttonColor (Zipper.label parent)) ]
                            { onPress = Just (Focus (Zipper.label parent).id), label = text (Zipper.label parent).title }
                        , current
                        ]
                  ]
                , viewAfter toggle parent
                ]
                |> column []
                |> inAncestors toggle parent

        Nothing ->
            current


buttonColor l =
    case l.isRoot of
        True ->
            darkBlue

        False ->
            charcoal


charcoal =
    Element.rgb 0.4 0.4 0.4


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )


horizontalPadding =
    paddingXY 12 0
