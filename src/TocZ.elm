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


viewZ : String -> Bool -> Zipper Label -> Element TocMsg
viewZ selectedTitle t z =
    let
        offset =
            if (Zipper.label z).isRoot then
                0

            else
                12
    in
    (List.concat
        [ viewBefore selectedTitle t z
        , [ viewSelf selectedTitle t (Zipper.tree z) ]
        , viewAfter selectedTitle t z
        ]
        |> Html.ul [ Attr.class "mm-ul-toc" ]
        |> inAncestors selectedTitle t z
    )
        |> (\x ->
                Html.div [ Attr.style "margin-left" "-24px" ] [ x ]
                    |> Element.html
           )


viewBefore : String -> Bool -> Zipper Label -> List (Html TocMsg)
viewBefore selectedTitle t z =
    List.map (viewNode selectedTitle t) (Zipper.siblingsBeforeFocus z)


viewAfter : String -> Bool -> Zipper Label -> List (Html TocMsg)
viewAfter selectedTitle t z =
    List.map (viewNode selectedTitle t) (Zipper.siblingsAfterFocus z)


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


viewSelf : String -> Bool -> Tree Label -> Html TocMsg
viewSelf selectedTitle toggle t =
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
    Html.li [ liClass selectedTitle l.title ]
        [ Html.span [] [ Html.text <| prefix l ++ l.title ]
        , Html.ul [ Attr.class "mm-ul-toc" ] (List.map (viewNode selectedTitle toggle) (Tree.children t))
        ]


liClass : String -> String -> Html.Attribute msg
liClass selectedTitle actualTitle =
    case selectedTitle == actualTitle of
        True ->
            Attr.class "mm-li-toc-selected"

        False ->
            Attr.class "mm-li-toc"


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewNode : String -> Bool -> Tree Label -> Html TocMsg
viewNode selectedTitle showAll t =
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
                [ Html.ul [ Attr.class "mm-ul-toc" ] (List.map (viewNode selectedTitle showAll) (Tree.children t)) ]

            else
                []
    in
    Html.li [ liClass selectedTitle l.title ]
        (Html.span
            [ Events.onClick (Focus l.id), Attr.class class ]
            [ Html.text <| prefix l ++ l.title ]
            :: xs
        )



--XXXXX


inAncestors : String -> Bool -> Zipper Label -> Html TocMsg -> Html TocMsg
inAncestors selectedTitle toggle zipper current =
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
                [ viewBefore selectedTitle toggle parent
                , [ Html.li [ liClass selectedTitle l.title ]
                        [ Html.span
                            [ Events.onClick (Focus (Zipper.label parent).id), Attr.class class ]
                            [ Html.text <| prefix l ++ (Zipper.label parent).title ]
                        , current
                        ]
                  ]
                , viewAfter selectedTitle toggle parent
                ]
                |> Html.ul [ Attr.class "mm-ul-toc" ]
                |> inAncestors selectedTitle toggle parent

        Nothing ->
            current


indexedMap : (Int -> a -> b) -> ( a, List a ) -> ( b, List b )
indexedMap f ( x, xs ) =
    ( f 0 x, List.indexedMap (\idx -> f (idx + 1)) xs )
