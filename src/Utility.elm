module Utility exposing (humanTimeHM, humanTimeHMS, normalize, stringOfPosix, humanDateUTC, wordCount, compress)
import List.Extra

import Time exposing(Posix)


wordCount str = List.length (String.words str) |> String.fromInt


humanTimeHM : Time.Zone -> Posix -> String
humanTimeHM zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)  |> String.padLeft 2 '0'
  in
   hour ++ ":" ++ minute


humanTimeHMS : Time.Zone -> Posix -> String
humanTimeHMS zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
    second = String.fromInt (Time.toSecond zone time)
  in
   hour ++ ":" ++ (minute |> String.padLeft 2 '0') ++ ":" ++ (second |> String.padLeft 2 '0')



humanDateUTC : Posix -> String
humanDateUTC time =
  let
    year   = String.fromInt (Time.toYear   Time.utc time)
    month = (Time.toMonth Time.utc time) |> monthToString
    day = String.fromInt (Time.toDay Time.utc time)
  in
   day ++ " " ++ month ++ ", " ++ year


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan -> "Jan"
        Time.Feb -> "Feb"
        Time.Mar -> "Mar"
        Time.Apr -> "Apr"
        Time.May -> "May"
        Time.Jun -> "Jun"
        Time.Jul -> "Jul"
        Time.Aug -> "Aug"
        Time.Sep -> "Sep"
        Time.Oct -> "Oct"
        Time.Nov -> "Nov"
        Time.Dec -> "Dec"

normalize: String -> String
normalize str =
    str
      |> String.words
      |> String.join "-"
      |> String.toLower

compress1: String -> String
compress1 str =
  let
    filteredWords = str
      |> String.words
      |> List.map String.toLower
      |> filterNoise
    result = List.Extra.uncons filteredWords
  in
    case result of
        Nothing -> String.toLower str
        Just (first, body) ->
            body
              |> List.map (String.left 2)
              |> Debug.log "(1)"
              |> fixup
              |> List.Extra.groupsOf 2
              |> Debug.log "(2)"
              |> List.map (\sublist -> sublist |> String.join "")
              |> (\x -> first::x)
              |> String.join "-"

compress: String -> String
compress str =
   str
      |> String.words
      |> List.map String.toLower
      |> filterNoise
      |> List.indexedMap shorten
      |> Debug.log "(1)"
      |> fixup
      |> List.Extra.groupsOf 2
      |> Debug.log "(2)"
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
        list ++ [""]

filterNoise : List String -> List String
filterNoise list =
    List.filter (\word -> not (List.member word lowInfoWords)) list

lowInfoWords = ["a", "the"]

stringOfPosix : Posix -> String
stringOfPosix posix =
    posix
      |> Time.posixToMillis
      |> (\x -> x // 1000)
      |> chunked


chunked : Int -> String
chunked k =
    let
        right = modBy 1000 k |> String.fromInt |> String.padLeft 3 '0'
        k2 = k // 1000
        middle = modBy 1000 k2 |> String.fromInt |> String.padLeft 3 '0'
        left = k2 // 1000 |> String.fromInt |> String.padLeft 3 '0'
    in
      [left, middle, right] |> String.join "-"