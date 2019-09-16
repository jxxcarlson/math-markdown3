module Utility exposing (humanTimeHM, humanTimeHMS)

import Time exposing(Posix)


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