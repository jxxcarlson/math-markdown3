module Config exposing (endpoint, hasuraToken, tick, wordsPerPage)


endpoint =
    "https://math-markdown.netlify.com"


hasuraToken =
    "GOc97wA7CCMm31H4UJHa-4pqdVoLf3l6gAwzczdHC"


wordsPerPage : Float
wordsPerPage =
    450.0


tick : Float
tick =
    1 * 1000
