module Utility exposing (boolAsString, compress, getId, humanDateUTC, humanTimeHM, humanTimeHMS, id0, intSlug, normalize, wordCount)

import Api.Scalar exposing (Id(..))
import List.Extra
import Prng.Uuid exposing (Uuid(..))
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Time exposing (Posix)


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


humanTimeHM : Time.Zone -> Posix -> String
humanTimeHM zone time =
    let
        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time) |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minute


humanTimeHMS : Time.Zone -> Posix -> String
humanTimeHMS zone time =
    let
        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time)

        second =
            String.fromInt (Time.toSecond zone time)
    in
    hour ++ ":" ++ (minute |> String.padLeft 2 '0') ++ ":" ++ (second |> String.padLeft 2 '0')


humanDateUTC : Posix -> String
humanDateUTC time =
    let
        year =
            String.fromInt (Time.toYear Time.utc time)

        month =
            Time.toMonth Time.utc time |> monthToString

        day =
            String.fromInt (Time.toDay Time.utc time)
    in
    day ++ " " ++ month ++ ", " ++ year


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


normalize : String -> String
normalize str =
    str
        |> String.words
        |> List.map (String.toLower >> silencePunctuation)
        |> filterEmpties
        |> String.join "-"


filterEmpties : List String -> List String
filterEmpties strListr =
    List.filter (\x -> x /= "") strListr


silencePunctuation : String -> String
silencePunctuation str =
    str
        |> String.split ""
        |> List.filter isAlphaNumOrWhiteSpace
        |> String.join ""


isAlphaNumOrWhiteSpace : String -> Bool
isAlphaNumOrWhiteSpace x =
    List.member x [ " ", "\n", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "z", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]


compress : String -> String
compress str =
    str
        |> String.words
        |> List.map String.toLower
        |> filterNoise
        |> List.map silencePunctuation
        |> List.indexedMap shorten
        |> fixup
        |> List.Extra.groupsOf 2
        |> List.indexedMap join
        |> String.join "-"


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
        list ++ [ "" ]


filterNoise : List String -> List String
filterNoise list =
    List.filter (\word -> not (List.member word lowInfoWords)) list


lowInfoWords =
    [ "a", "about", "the", "in", "is", "on", "of", "for", "to", "from", "with", "without", "that", "this", "and", "or" ]


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
