module YamlX exposing (decodePair, foo, parsePair, parseStringToChar)

import Parser exposing ((|.), (|=), Parser)
import Yaml.Decode as Decode exposing (Decoder)


{-|

    import Yaml.Decode as Decode

    Decode.fromString decodePair "(test, 43) "
    --> Ok ("test", 43)

-}
decodePair : Decoder ( String, Int )
decodePair =
    Decode.string
        |> Decode.andThen decoderPairAux


decoderPairAux : String -> Decoder ( String, Int )
decoderPairAux str =
    str
        |> Debug.log "Parser input (2)"
        --  |> foo
        |> Parser.run parsePair
        |> handleParseResult


handleParseResult : Result (List Parser.DeadEnd) ( String, Int ) -> Decoder ( String, Int )
handleParseResult result =
    case Debug.log "hpr (2)" result of
        Ok pair ->
            Decode.succeed pair

        Err _ ->
            Decode.fail "Error parsing pair"


{-|

    foo "bar"
    --> Ok ("test", 43)

-}
foo : String -> Result (List Parser.DeadEnd) ( String, Int )
foo str =
    Parser.run (Parser.succeed ( "test", 43 )) str


{-|

    import Parser

    Parser.run parsePair "(test, 43)"

-}
parsePair : Parser ( String, Int )
parsePair =
    Parser.succeed (\s k -> ( s, k ))
        |. Parser.symbol "("
        |. Parser.spaces
        |= parseStringToChar ','
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ")"


{-|

    import Parser

    Parser.run (parseStringToChar '.') "test."
    --> Ok "test"

-}
parseStringToChar : Char -> Parser String
parseStringToChar endChar =
    (Parser.getChompedString <|
        Parser.succeed identity
            |. parseWhile (\c -> c /= endChar)
    )
        |> Parser.map String.trim


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    Parser.chompWhile accepting |> Parser.getChompedString
