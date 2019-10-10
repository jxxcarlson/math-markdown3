module TocZ exposing (Id, Label, Model, Msg(..), content, focus, inAncestors, indexedMap, initialModel, main, update, view, viewAfter, viewBefore, viewNode, viewSelf, viewZ)

--import Browser
--import Html exposing (Html)
--import Html.Attributes as Attr
--import Html.Events as Events

import Element exposing (Element)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



--
--type alias Model =
--    { toggle : Bool
--    , data : Zipper Label
--    }
--


type alias Id =
    ( Int, Int )


type alias Label =
    { id : Id
    , content : String
    }



--
--initialModel : Model
--initialModel =
--    { data = content
--    , toggle = False
--    }


type Msg
    = Focus Id
    | Toggle



--update : Msg -> Model -> Model
--update msg model =
--    case msg of
--        Focus id ->
--            { model | data = focus id model.data }
--
--        Toggle ->
--            { model | toggle = not model.toggle }


focus : Id -> Zipper Label -> Zipper Label
focus id zipper =
    zipper
        |> Zipper.findFromRoot (\l -> l.id == id)
        |> Maybe.withDefault zipper



--view : Model -> Html Msg
--view model =
--    Html.div []
--        [ Html.button [ Events.onClick Toggle ] [ Html.text "Expand/Collapse" ]
--        , viewZ model.toggle model.data
--        ]


viewZ : Bool -> Zipper Label -> Element Msg
viewZ t z =
    List.concat
        [ viewBefore t z
        , [ viewSelf t (Zipper.tree z) ]
        , viewAfter t z
        ]
        |> Element.column []
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
    Html.li []
        [ Html.span [ Attr.class "current" ] [ Html.text l.content ]
        , Html.ul [] (List.map (viewNode toggle) (Tree.children t))
        ]


viewNode : Bool -> Tree Label -> Html Msg
viewNode showAll t =
    let
        l =
            Tree.label t

        xs =
            if showAll then
                [ Html.ul [] (List.map (viewNode showAll) (Tree.children t)) ]

            else
                []
    in
    Html.li []
        (Html.span
            [ Events.onClick (Focus l.id) ]
            [ Html.text l.content ]
            :: xs
        )


inAncestors : Bool -> Zipper Label -> Element Msg -> Element Msg
inAncestors toggle zipper current =
    case Zipper.parent zipper of
        Just parent ->
            List.concat
                [ viewBefore toggle parent
                , [ Html.li []
                        [ Html.span
                            [ Events.onClick (Focus (Zipper.label parent).id) ]
                            [ Html.text (Zipper.label parent).content ]
                        , current
                        ]
                  ]
                , viewAfter toggle parent
                ]
                |> Html.ul []
                |> inAncestors toggle parent

        Nothing ->
            current



--
--main : Program () Model Msg
--main =
--    Browser.sandbox
--        { init = initialModel
--        , view = view
--        , update = update
--        }


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
