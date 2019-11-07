module Utility.Time exposing
    ( humanDateUTC
    , humanTimeHM
    , humanTimeHMS
    )

import Time exposing (Posix)


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
