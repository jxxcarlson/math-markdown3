module Utility exposing
    ( boolAsString
    , getId
    , id0
    , intSlug
    , pxFromFloat
    , uuids
    , wordCount
    )

import Prng.Uuid exposing (Uuid(..))
import Random.Pcg.Extended exposing (Seed, initialSeed, step)


boolAsString : Bool -> String
boolAsString bit =
    if bit then
        "Yes"

    else
        "No"


id0 =
    step Prng.Uuid.generator (initialSeed 0 [ 1, 2, 3, 4 ]) |> Tuple.first


uuids =
    let
        ( id1, seed1 ) =
            step Prng.Uuid.generator (initialSeed 0 [ 1, 2, 3, 4 ])

        ( id2, seed2 ) =
            step Prng.Uuid.generator seed1

        ( id3, seed3 ) =
            step Prng.Uuid.generator seed2

        ( id4, seed4 ) =
            step Prng.Uuid.generator seed3

        ( id5, seed5 ) =
            step Prng.Uuid.generator seed4

        ( id6, seed6 ) =
            step Prng.Uuid.generator seed5
    in
    [ id1, id2, id3, id4, id5, id6 ]


getId : Int -> Uuid
getId k =
    List.drop (k - 1) uuids
        |> List.head
        |> Maybe.withDefault id0


wordCount str =
    List.length (String.words str) |> String.fromInt


join : Int -> List String -> String
join k list =
    if k < 1 then
        String.join "-" list

    else
        String.join "" list


shorten : Int -> String -> String
shorten k str =
    if k < 2 then
        str

    else
        String.left 2 str


fixup : List String -> List String
fixup list =
    if modBy 2 (List.length list) == 0 then
        list

    else
        list ++ [ "1" ]


msp : Int -> String
msp seconds =
    seconds
        |> (\x -> x // 86400)
        |> (\x -> modBy 1000 x)
        |> String.fromInt
        |> String.padLeft 3 '0'


lsp : Int -> String
lsp seconds =
    let
        s =
            modBy 86400 seconds

        a =
            s // 1000 |> String.fromInt |> String.padLeft 2 '0'

        b =
            modBy 1000 s |> String.fromInt |> String.padLeft 3 '0'
    in
    a ++ "-" ++ b


intSlug : Int -> String
intSlug seconds =
    msp seconds ++ "-" ++ lsp seconds


chunked : Int -> String
chunked k =
    let
        right =
            modBy 1000 k |> String.fromInt |> String.padLeft 3 '0'

        k2 =
            k // 1000

        middle =
            modBy 1000 k2 |> String.fromInt |> String.padLeft 3 '0'

        left =
            k2 // 1000 |> String.fromInt |> String.padLeft 3 '0'
    in
    [ left, middle, right ] |> String.join "-"


pxFromFloat : Float -> String
pxFromFloat f =
    f
        |> round
        |> String.fromInt
        |> (\s -> s ++ "px")
