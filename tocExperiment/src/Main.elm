module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias Model =
    { toggle : Bool
    , data : Zipper Label
    }


type alias Id =
    ( Int, Int )


type alias Label =
    { id : Id
    , content : String
    }


initialModel : Model
initialModel =
    { data = content
    , toggle = False
    }


type Msg
    = Focus Id
    | Toggle


update : Msg -> Model -> Model
update msg model =
    case msg of
        Focus id ->
            { model | data = focus id model.data }

        Toggle ->
            { model | toggle = not model.toggle }


focus : Id -> Zipper Label -> Zipper Label
focus id zipper =
    zipper
        |> Zipper.findFromRoot (\l -> l.id == id)
        |> Maybe.withDefault zipper


view : Model -> Html Msg
view model =
    layout [ padding 30, Font.size 12 ] <|
        column [ spacing 12 ]
            [ el [ Font.bold, Font.size 14 ] (text "Table of Contents")
            , expandCollapseButton
            , viewZ model.toggle model.data
            ]


expandCollapseButton =
    Input.button []
        { onPress = Just Toggle
        , label = text "Expand/Collapse"
        }


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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


content : Zipper Label
content =
    ( Tree.singleton "Introduction"
    , [ Tree.tree "Energy"
            [ Tree.tree "Conversations"
                [ Tree.singleton "A first conversation"
                , Tree.singleton "Energy, a second conversation"
                ]
            , Tree.singleton "The meteor that killed the dinosaurs"
            , Tree.singleton "The Parable of the Neutrino"
            ]
      , Tree.tree "Time"
            [ Tree.singleton "The Pendulum"
            , Tree.singleton "Atomic clocks"
            ]
      , Tree.tree "Speed and Acceleration"
            [ Tree.singleton "Galileo's Experiment"
            , Tree.singleton "Speed of sound"
            , Tree.tree "Speed of Light"
                [ Tree.singleton "Roemer's experiment"
                , Tree.singleton "Bradley's experiment"
                , Tree.singleton "Michelson's experiment"
                ]
            ]
      , Tree.singleton "Distance"
      , Tree.singleton "Light"
      , Tree.singleton "Heat"
      , Tree.singleton "Sound"
      ]
    )
        |> indexedMap
            (\tIdx ->
                Tree.indexedMap
                    (\nIdx label ->
                        { id = ( tIdx, nIdx )
                        , content = label
                        }
                    )
            )
        |> (\( t, ts ) -> Zipper.fromForest t ts)


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )
