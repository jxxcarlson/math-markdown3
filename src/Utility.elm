module Utility exposing (humanTimeHM, humanTimeHMS, normalize, stringOfPosix, humanDateUTC, wordCount)

import Time exposing(Posix)


wordCount str = List.length (String.words str) |> String.fromInt


humanTimeHM : Time.Zone -> Posix -> String
humanTimeHM zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
  in
   hour ++ ":" ++ minute


humanTimeHMS : Time.Zone -> Posix -> String
humanTimeHMS zone time =
  let
    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
    second = String.fromInt (Time.toSecond zone time)
  in
   hour ++ ":" ++ minute ++ ":" ++ second



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


stringOfPosix : Posix -> String
stringOfPosix posix =
    posix
      |> Time.posixToMillis
      |> (\x -> x // 1000)
      |> String.fromInt