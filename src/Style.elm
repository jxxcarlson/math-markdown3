module Style exposing
    ( activeButtonStyle
    , basicButtonsStyle
    , black
    , blue
    , brighterBlue
    , buttonFontSize
    , buttonGrey
    , buttonSelected
    , buttonSelected_
    , charcoal
    , darkBlue
    , darkRed
    , green
    , grey
    , inputStyle
    , lightGrey
    , mainColumn
    , makeGrey
    , mediumBlue
    , mouseDownColor
    , multiline
    , preWrap
    , red
    , signInColumn
    , standardButton
    , textInputStyle
    , textInputStyleSimple
    , white
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


mainColumn w h =
    [ paddingXY 8 8, spacing 12, width w, height h, clipY, clipX ]


signInColumn =
    mainColumn fill fill ++ [ spacing 12, paddingXY 72 36, Background.color (makeGrey 0.4) ]



-- INPUT --


inputStyle w =
    [ width (px w)
    , height (px 30)
    , moveUp 4
    , Background.color (makeGrey 0.9)
    , Font.color (makeGrey 0.1)
    , Font.size 14
    , Border.width 1
    ]


multiline w h =
    [ width (px w)
    , height (px h)
    , Background.color (makeGrey 0.8)
    , Font.color black
    , Font.size 12
    , Border.width 2
    , scrollbarY
    ]



-- BUTTONS --


standardButton : List (Element.Attr () msg)
standardButton =
    [ Background.color white, Font.color black, paddingXY 10 6 ] ++ basicButtonsStyle


basicButtonsStyle : List (Element.Attr () msg)
basicButtonsStyle =
    [ buttonFontSize
    , pointer
    , mouseDown [ buttonFontSize, Background.color darkRed, Font.color white ]
    ]


activeButtonStyle : List (Element.Attr () msg)
activeButtonStyle =
    [ Background.color charcoal, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle


buttonFontSize =
    Font.size 16


mouseDownColor =
    Element.rgb 0.7 0.1 0.1


buttonSelected =
    buttonSelected_ buttonGrey red


buttonSelected_ : Color -> Color -> Int -> Bool -> List (Attr () msg)
buttonSelected_ color color2 width_ bit =
    [ case bit of
        False ->
            Background.color color

        True ->
            Background.color color2
    , Font.color white
    , width (px width_)
    , height (px 25)
    , Font.size 12
    ]


preWrap =
    Element.htmlAttribute (Html.Attributes.attribute "white-space" "pre-wrap")


textInputStyle w h =
    [ preWrap
    , height <| px <| round h
    , width <| px <| round w
    , clipX
    , paddingXY 12 12
    , Font.size 13
    , paddingXY 8 20
    , Background.color lightGrey
    , Border.width 2
    ]


textInputStyleSimple w h =
    [ preWrap
    , height <| px <| round h
    , width <| px <| round w
    , clipX
    , Font.size 13
    , paddingXY 4 4
    , Background.color lightGrey
    , Border.width 2
    ]



-- COLOR --


darkRed =
    Element.rgb 0.5 0.0 0.0


darkBlue =
    Element.rgb 0.0 0.0 0.6



-- COLORS --


makeGrey : Float -> Color
makeGrey g =
    Element.rgb g g g


lightGrey : Color
lightGrey =
    makeGrey 0.95


buttonGrey : Color
buttonGrey =
    grey 0.5


red : Color
red =
    Element.rgb 0.4 0.1 0.1


white : Color
white =
    Element.rgb 1 1 1


blue : Color
blue =
    Element.rgb 0.1 0.1 0.4


brighterBlue : Color
brighterBlue =
    Element.rgb 0.1 0.1 0.7


mediumBlue : Color
mediumBlue =
    Element.rgb 0.1 0.1 0.5


green : Color
green =
    Element.rgb 0 0.5 0


grey : Float -> Color
grey g =
    Element.rgb g g g


charcoal : Color
charcoal =
    grey 0.3


black : Color
black =
    grey 0.1
