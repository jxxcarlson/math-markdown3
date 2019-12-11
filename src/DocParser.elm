module DocParser exposing (getLeadingSpace, parseHeading, parseLatexHeading, parseMarkdownHeading, parseWhile)

import Parser exposing ((|.), (|=), Parser, chompWhile, getChompedString, spaces, succeed, symbol)


{-|

    parseHeading "# Intro"
    --> Just "Intro"

    parseHeading "\\section{Intro}"
    --> Just "Intro"

    parseHeading "   # Intro"
    --> Just "Intro"

    parseHeading "   \\section{Intro}"
    --> Just "Intro"

-}
parseHeading : String -> Maybe String
parseHeading str =
    case Parser.run parseHeading_ str of
        Ok result ->
            Just result

        Err _ ->
            Nothing


parseHeading_ : Parser String
parseHeading_ =
    succeed identity
        |. spaces
        |= Parser.oneOf [ parseLatexHeading, parseMarkdownHeading ]


{-|

    import Parser

    Parser.run parseLatexHeading "\\section{Intro}\n"
    --> Ok "Intro"

-}
parseLatexHeading : Parser String
parseLatexHeading =
    (succeed identity
        |. symbol "\\section{"
        |= parseWhile (\c -> c /= '}')
        |. symbol "}"
    )
        |> Parser.map String.trim


{-|

    import Parser

    Parser.run parseMarkdownHeading "# Intro\n"
    --> Ok "Intro"

-}
parseMarkdownHeading : Parser String
parseMarkdownHeading =
    (succeed identity
        |. spaces
        |. symbol "# "
        |= parseWhile (\c -> c /= '\n')
    )
        |> Parser.map (String.replace "#" "" >> String.trim)


parseLeadingSpace : Parser String
parseLeadingSpace =
    getChompedString <|
        succeed identity
            |. parseWhile (\c -> c == ' ')


getLeadingSpace : String -> Maybe String
getLeadingSpace str =
    case Parser.run parseLeadingSpace str of
        Ok leadingSpace ->
            Just leadingSpace

        Err _ ->
            Nothing


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString
