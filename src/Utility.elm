module Utility exposing (humanTimeHM, humanTimeHMS, normalize, stringOfPosix, humanDateUTC, wordCount, compress, posixSlug)
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
  List.member x [" ", "\n", "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","z","y","z","0","1","2","3","4","5","6","7","8","9"]

compress: String -> String
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
        list ++ [""]

filterNoise : List String -> List String
filterNoise list =
    List.filter (\word -> not (List.member word lowInfoWords)) list

lowInfoWords = ["a", "the"]


msp : Int -> String
msp seconds =
    seconds
      |> (\x -> x // 86400)
      |> (\x -> modBy 1000 x )
      |> String.fromInt

lsp : Int -> String
lsp seconds =
   let
      s = modBy 86400 seconds
      a = s // 1000 |> String.fromInt
      b = modBy 1000 s |> String.fromInt
   in
     a ++ "-" ++ b

posixSlug : Posix -> String
posixSlug posix =
    posix
      |> Time.posixToMillis
      |> (\x -> x // 1000)
      |> intSlug

intSlug : Int -> String
intSlug seconds =
    msp seconds ++ "-" ++ lsp seconds

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