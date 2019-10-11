module TozZ exposing (Id, Label, Msg(..), darkRed, focus, inAncestors, indexedMap, viewAfter, viewBefore, viewNode, viewSelf, viewZ)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias Id =
    ( Int, Int )


type alias Label =
    { id : Id
    , content : String
    }


type Msg
    = Focus Id
    | Toggle


focus : Id -> Zipper Label -> Zipper Label
focus id zipper =
    zipper
        |> Zipper.findFromRoot (\l -> l.id == id)
        |> Maybe.withDefault zipper


viewZ : Bool -> Zipper Label -> Element Msg
viewZ t z =
    List.concat
        [ viewBefore t z
        , [ viewSelf t (Zipper.tree z) ]
        , viewAfter t z
        ]
        |> column []
        |> inAncestors t z


viewBefore : Bool -> Zipper Label -> List (Element Msg)
viewBefore t z =
    List.map (viewNode t) (Zipper.siblingsBeforeFocus z)


viewAfter : Bool -> Zipper Label -> List (Element Msg)
viewAfter t z =
    List.map (viewNode t) (Zipper.siblingsAfterFocus z)


viewSelf : Bool -> Tree Label -> Element Msg
viewSelf toggle t =
    let
        l =
            Tree.label t
    in
    column []
        [ el [ Font.bold, Font.color darkRed ] (text l.content)
        , column [ paddingXY 12 0 ] (List.map (viewNode toggle) (Tree.children t))
        ]


darkRed =
    rgb 0.5 0.0 0.0


viewNode : Bool -> Tree Label -> Element Msg
viewNode showAll t =
    let
        l =
            Tree.label t

        xs =
            if showAll then
                [ column [ paddingXY 12 0 ] (List.map (viewNode showAll) (Tree.children t)) ]

            else
                []
    in
    column []
        (Input.button []
            { onPress = Just (Focus l.id)
            , label = el [] (text l.content)
            }
            :: xs
        )


inAncestors : Bool -> Zipper Label -> Element Msg -> Element Msg
inAncestors toggle zipper current =
    case Zipper.parent zipper of
        Just parent ->
            List.concat
                [ viewBefore toggle parent
                , [ column [ paddingXY 12 0 ]
                        [ Input.button []
                            { onPress = Just (Focus (Zipper.label parent).id), label = text (Zipper.label parent).content }
                        , current
                        ]
                  ]
                , viewAfter toggle parent
                ]
                |> column []
                |> inAncestors toggle parent

        Nothing ->
            current


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )
