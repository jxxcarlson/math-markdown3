module Router exposing (Route(..), routeParser)

import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = ArticleBySlug String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ArticleBySlug (s "article-ref" </> string)
        ]
