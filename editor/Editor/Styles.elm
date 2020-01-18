module Editor.Styles exposing (editorStyles)

import Html exposing (Html, text)
import String.Interpolate exposing (interpolate)


style : List (Html.Attribute msg) -> List (Html msg) -> Html msg
style =
    Html.node "style"


editorStyles : StyleConfig -> Html msg
editorStyles styleConfig =
    style [] [ text (styleText styleConfig) ]


type alias StyleConfig =
    { editorWidth : Float
    , editorHeight : Float
    , lineHeight : Float
    }


type alias StyleParams =
    { editorWidth : String
    , editorHeight : String
    , lineHeight : String
    , fontSize : String
    , sliderXOffset : String
    , sliderYOffset : String
    }


getStyleParams : StyleConfig -> StyleParams
getStyleParams c =
    { editorWidth = String.fromFloat c.editorWidth
    , editorHeight = String.fromFloat c.editorHeight
    , lineHeight = String.fromFloat c.lineHeight
    , fontSize = String.fromFloat (0.8 * c.lineHeight)
    , sliderXOffset = String.fromFloat <| c.editorWidth + 50
    , sliderYOffset = String.fromFloat <| 1.04348 * c.editorHeight - 90.87 - 20
    }


styleText : StyleConfig -> String
styleText styleConfig =
    let
        s =
            getStyleParams styleConfig
    in
    interpolate styleTemplate
        [ s.editorWidth
        , s.sliderXOffset
        , s.fontSize
        , s.lineHeight
        , s.sliderYOffset -- String.fromFloat <| sliderOffsetY styleConfig.numberOfLines styleConfig.lineHeight -- {4}
        , s.sliderXOffset -- String.fromFloat <| editorHeight -- {5}
        , s.editorHeight -- {6}
        ]



--sliderOffsetX : Int -> Float
--sliderOffsetX k =
--    (k + 50)
--        |> toFloat
--        |> (\x -> 1.0 * x)
--
--
--sliderOffsetY : Int -> Float -> Float
--sliderOffsetY numberOfLines lineHeight =
--    0.8 * toFloat numberOfLines * lineHeight


styleTemplate : String
styleTemplate =
    """

body { font-size: {2}px;
       line-height: {3}px;}

.elm-editor-container {
  font-family: monospace;
  width: {0}px;
  user-select: none;
  -webkit-user-select: none;
  display: flex;
  // overflow-x : scroll;
  // overflow-y : scroll;
  // height: {6}px;
}

.elm-editor-container:focus {
  outline: none;
    // background-color : lightblue;
}

.elm-editor-gutter {
  display: flex;
  flex-direction: column;
  flex-shrink: 0;
}

.elm-editor-lines {
  flex-grow: 1;
}

.elm-editor-line-number {
  display: inline-block;
  width: 35px;
  padding-right: 5px;
  text-align: right;
  background-color: lightgray;
  cursor: default;
}

.elm-editor-line {
  cursor: text;
}

.elm-editor-line__gutter-padding {
  width: 5px;
}

.elm-editor-line__character--has-cursor {
  position: relative;
}

.elm-editor-line__character--selected {
  background-color: cornflowerblue;
  color: white;
}

.elm-editor-cursor {
  position: absolute;
  border-left: 16px solid #990000;
  opacity: 0.2;
  left: 0;
  height: 100%;
}

.elm-editor-container:focus .elm-editor-cursor {
  animation: 1s blink step-start infinite;
  border-left: 4px solid #333333;
}

@keyframes blink {
  from, to {
    opacity: 0;
  }
  50% {
    opacity: 1;s
  }
}


body {
    font-family: sans-serif;

    }

.center-column {
    display: flex;
    flex-direction: column;
    align-items: center;
    background-color: lightblue; //: #eeeeee;
    }

#editor-container {
    text-align: left;
9
    }

.input-range-labels-container { visibility: hidden }



.input-range-container {
     transform: rotate(-270deg) translateY(-{1}px) translateX({4}px)
}

"""
