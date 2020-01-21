module TocZ exposing
    ( Id
    , Label
    , TocMsg(..)
    , focus
    , inAncestors
    , indexedMap
    , viewAfter
    , viewBefore
    , viewNode
    , viewSelf
    , viewZ
    )

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Prng.Uuid as Uuid exposing (Uuid)
import Toc exposing (TocItem)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



{-

   Ilias' Ellie: https://ellie-app.com/6SZ5wnf38Vpa1

-}


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
        |> Html.ul [ Attr.class "mm-ul-toc" ]
        |> inAncestors t z
    )
        |> (\x ->
                Html.div [ Attr.style "margin-left" "-24px" ] [ x ]
                    |> Element.html
           )


viewBefore : Bool -> Zipper Label -> List (Html TocMsg)
viewBefore t z =
    List.map (viewNode t) (Zipper.siblingsBeforeFocus z)


viewAfter : Bool -> Zipper Label -> List (Html TocMsg)
viewAfter t z =
    List.map (viewNode t) (Zipper.siblingsAfterFocus z)


prefix l =
    case ( l.isRoot, l.hasChildren ) of
        ( True, _ ) ->
            "    "

        ( False, Just True ) ->
            "+ "

        ( False, Just False ) ->
            "   "

        ( False, Nothing ) ->
            "   "


viewSelf : Bool -> Tree Label -> Html TocMsg
viewSelf toggle t =
    let
        l =
            Tree.label t

        class =
            case l.isRoot of
                True ->
                    "mm-toc-root"

                False ->
                    "mm-toc-item"
    in
    Html.li [ Attr.class "mm-li-toc" ]
        [ Html.span [ Attr.class class ] [ Html.text <| prefix l ++ l.title ]
        , Html.ul [ Attr.class "mm-ul-toc" ] (List.map (viewNode toggle) (Tree.children t))
        ]



--    column []
--        [ el [ paddingEach { edges | bottom = 2, top = 2 }, Font.bold, Font.color color ] (text <| prefix l ++ l.title)
--        , column [ horizontalPadding ] (List.map (viewNode toggle) (Tree.children t))
--        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewNode : Bool -> Tree Label -> Html TocMsg
viewNode showAll t =
    let
        l =
            Tree.label t

        class =
            case l.isRoot of
                True ->
                    "mm-toc-root"

                False ->
                    "mm-toc-item"

        xs =
            if showAll then
                [ Html.ul [ Attr.class "mm-ul-toc" ] (List.map (viewNode showAll) (Tree.children t)) ]

            else
                []
    in
    Html.li [ Attr.class "mm-li-toc" ]
        (Html.span
            [ Events.onClick (Focus l.id), Attr.class class ]
            [ Html.text <| prefix l ++ l.title ]
            :: xs
        )



--XXXXX


inAncestors : Bool -> Zipper Label -> Html TocMsg -> Html TocMsg
inAncestors toggle zipper current =
    case Zipper.parent zipper of
        Just parent ->
            let
                l =
                    Zipper.label parent

                class =
                    case l.isRoot of
                        True ->
                            "root"

                        False ->
                            ""
            in
            List.concat
                [ viewBefore toggle parent
                , [ Html.li [ Attr.class "mm-li-toc" ]
                        [ Html.span
                            [ Events.onClick (Focus (Zipper.label parent).id), Attr.class class ]
                            [ Html.text <| prefix l ++ (Zipper.label parent).title ]
                        , current
                        ]
                  ]
                , viewAfter toggle parent
                ]
                |> Html.ul [ Attr.class "mm-ul-toc" ]
                |> inAncestors toggle parent

        Nothing ->
            current



--            List.concat
--                [ viewBefore toggle parent
--                , [ column []
--                        [ Input.button [ buttonPadding, Font.color (buttonColor (Zipper.label parent)) ]
--                            { onPress = Just (Focus (Zipper.label parent).id), label = text <| prefix l ++ (Zipper.label parent).title }
--                        , current
--                        ]
--                  ]
--                , viewAfter toggle parent
--                ]
--                |> column []
--                |> inAncestors toggle parent
--
--        Nothing ->
--            current


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )
