module Editor exposing (selectionDecoder)

import Json.Decode as D exposing (Decoder)


selectionDecoder : Decoder String
selectionDecoder =
    D.field "selection" D.string
