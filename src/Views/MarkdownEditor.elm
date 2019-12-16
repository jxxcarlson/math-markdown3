module Views.MarkdownEditor exposing (editor, onBlur, onChanged, onFocus, value)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode


editor : List (Html.Attribute msg) -> Html msg
editor attributes =
    Html.node "markdown-editor" attributes []


value : String -> Html.Attribute msg
value code =
    Html.Attributes.property "editorValue" <|
        Json.Encode.string code


onChanged : (String -> msg) -> Html.Attribute msg
onChanged toMsg =
    Html.Events.on "editorChanged" <|
        Json.Decode.map toMsg <|
            Json.Decode.at [ "target", "editorValue" ] Json.Decode.string


onFocus : msg -> Html.Attribute msg
onFocus msg =
    Html.Events.on "editorFocus" <|
        Json.Decode.succeed msg


onBlur : msg -> Html.Attribute msg
onBlur msg =
    Html.Events.on "editorBlur" <|
        Json.Decode.succeed msg
