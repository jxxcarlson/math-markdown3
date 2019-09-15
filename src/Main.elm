module Main exposing (main)

import Parse
import Browser
import Html exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Markdown.Elm
import Html.Attributes
import Markdown.Option exposing (Option(..))
import Random
import Strings
import Style exposing (..)
import Color


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model =
    { sourceText : String
    , counter : Int
    , seed : Int
    , option : Option
    }


type Msg
    = Clear
    | GetContent String
    | GenerateSeed
    | NewSeed Int
    | RestoreText
    | RefreshText
    | SelectStandard
    | SelectExtended
    | SelectExtendedMath
    | InputNotes String


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { sourceText = Strings.initialText
            , counter = 0
            , seed = 0
            , option = ExtendedMath
            }
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetContent str ->
            ( { model
                | sourceText = str
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear ->
            ( { model
                | sourceText = ""
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        RestoreText ->
            ( { model
                | counter = model.counter + 1
                , sourceText = Strings.initialText
              }
            , Cmd.none
            )

        RefreshText ->
            ( { model
                | counter = model.counter + 1
              }
            , Cmd.none
            )

        SelectStandard ->
            ( { model
                | option = Standard
              }
            , Cmd.none
            )

        SelectExtended ->
            ( { model
                | option = Extended
              }
            , Cmd.none
            )

        SelectExtendedMath ->
            ( { model
                | option = ExtendedMath
              }
            , Cmd.none
            )

        InputNotes str ->

           ( {model | sourceText = str }, Cmd.none )


--
-- VIEW FUNCTIONS
---

type alias RenderedDocumentRecord msg = { document : Html msg, title : Html msg, toc : Html msg }

view : Model -> Html Msg
view model =
    Element.layout []  (display model)



display : Model -> Element Msg
display model =
  let
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option model.sourceText
  in
    column [ paddingXY 30 0]
        [  header model rt
         , row [spacing 10] [ editor model, renderedSource model rt ]
         , footer model
         ]

header : Model -> RenderedDocumentRecord msg -> Element msg
header model rt =
    row [ height (px 45), width fill, Background.color charcoal, paddingXY 30 0] [
     el [moveRight 400 , Font.size 14, Font.color white, width (px 400)] (rt.title |> Element.html)]

footer : Model -> Element Msg
footer model =
     row [height (px 30), width fill, spacing 10, Background.color charcoal, paddingXY 30 0]
       [
                   clearButton 60
                  , standardMarkdownButton model 80
                  , extendedMarkdownButton model 80
                  , extendedMathMarkdownButton model 93 ]


editor : Model -> Element Msg
editor model =
    column []
            [ Element.Keyed.el []
                ( String.fromInt model.counter
                , Input.multiline (textInputStyle)
                    { onChange = InputNotes
                    , text = model.sourceText
                    , placeholder = Nothing
                    , label = Input.labelBelow [ Font.size 0, Font.bold ] (Element.text "")
                    , spellcheck = False
                    }
                )
            ]



renderedSource : Model -> RenderedDocumentRecord msg -> Element msg
renderedSource model rt =
    let
        token =
            String.fromInt model.counter

    in
      row [spacing 10] [
        Element.Keyed.column [width (px 500), height (px 620), scrollbarY, Font.size 12, paddingXY 20 0]
           [ ( token, rt.document |> Element.html ) ]
        , Element.column [width (px 300), height (px 620), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (makeGrey 0.9)]
                                    [ rt.toc |> Element.html  ]
      ]


lightGrey =
    makeGrey 0.95


---- BUTTONS --


clearButton width =
    Input.button (buttonStyleSelected width False)
                  { onPress = Just Clear, label = el [centerX] (Element.text "Clear")}


standardMarkdownButton model width =
       let
                bit = (model.option == Standard)
       in
            Input.button (buttonStyleSelected width bit)
              { onPress = Just SelectStandard, label = el [centerX] (Element.text "Standard")}


extendedMarkdownButton model width =
    let
            bit = (model.option == Extended)
    in
        Input.button (buttonStyleSelected width bit)
          { onPress = Just SelectExtended, label = el [centerX] (Element.text "Extended")}


extendedMathMarkdownButton model width =
    let
        bit = (model.option == ExtendedMath)
    in
    Input.button (buttonStyleSelected width bit)
      { onPress = Just SelectExtendedMath, label = el [centerX] (Element.text "ExtendedMath")}



buttonStyleSelected = buttonStyleSelected_ blue red

buttonStyleSelected_ : Color -> Color -> Int -> Bool -> List (Attr () msg)
buttonStyleSelected_ color color2 width_ bit =
    [ case bit of
        False -> Background.color color
        True -> Background.color color2

    , Font.color white
    , width (px width_)
    , height (px 25)
    , Font.size 12
    , centerX
    ]

textInputStyle =
    [ preWrap
    , height(px 620)
    , width <| px <| 400
    , clipX
    , paddingXY 12 12
    , Font.size 13
    , paddingXY 8 20
    , Background.color lightGrey
    ,  Border.width 2
    ]

preWrap =
    Element.htmlAttribute (Html.Attributes.attribute "white-space" "pre-wrap")

makeGrey g =
    Element.rgb g g g


blue = Element.rgb 0 0 0.7

red =  Element.rgb 0.6 0 0

white = Element.rgb 1 1 1

grey g = Element.rgb g g g

charcoal = grey 0.3

black = grey 0