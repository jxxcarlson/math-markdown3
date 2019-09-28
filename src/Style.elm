module Style exposing (..)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


mainColumn w h =
    [ paddingXY 8 8, spacing 12, width w, height h, clipY, clipX ]


mainColumnX =
    mainColumn fill fill ++ [ spacing 12, padding 12, Background.color (makeGrey 0.4) ]

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

headerButton : List (Element.Attr () msg)
headerButton =
    [ Background.color white, Font.color black, paddingXY 10 6 ] ++ basicButtonsStyle


basicButtonsStyle =
    [ buttonFontSize
    , pointer
    , mouseDown [ buttonFontSize, Background.color mouseDownColor ]
    ]


activeButtonStyle : List (Element.Attr () msg)
activeButtonStyle =
    [ Background.color darkBlue, Font.color white, Element.paddingXY 10 6 ] ++ basicButtonsStyle

buttonFontSize =
    Font.size 16

mouseDownColor =
    Element.rgb 0.7 0.1 0.1

-- COLOR --


darkRed =
    Element.rgb 0.5 0.0 0.0


darkBlue =
    Element.rgb 0.0 0.0 0.6


-- COLORS --

makeGrey g =
    Element.rgb g g g


lightGrey =
    makeGrey 0.95

buttonGrey = grey 0.5

red =  Element.rgb 0.4 0.1 0.1

white = Element.rgb 1 1 1

blue = Element.rgb 0.1 0.1 0.4

grey g = Element.rgb g g g

charcoal = grey 0.3

black = grey 0.1