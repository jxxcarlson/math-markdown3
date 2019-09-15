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


view : Model -> Html Msg
view model =
    Element.layout []  (display model)



display : Model -> Element Msg
display model =
    column [spacing 10]
        [ row [spacing 10] [
              editor model
            , renderedSource model
          ]
          , row [  ] [ clearButton 60, standardMarkdownButton model 100, extendedMarkdownButton model 100, extendedMathMarkdownButton model 140 ]
         ]

--
--label text_ =
--    p labelStyle [ Html.text text_ ]
--

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

textInputStyle =
    [ preWrap
    , height(px 650)
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


renderedSource : Model -> Element Msg
renderedSource model =
    let
        token =
            String.fromInt model.counter
    in
    Element.Keyed.column
        [width (px 500), height (px 650), scrollbarY, Font.size 12, paddingXY 20 0]
        [ ( token, Markdown.Elm.toHtml ExtendedMath model.sourceText |> Element.html ) ]


lightGrey =
    makeGrey 0.95


---- BUTTONS --


clearButton width =
    Input.button (buttonStyleSelected width False)
                  { onPress = Just Clear, label = el [] (Element.text "Clear")}


standardMarkdownButton model width =
       let
                bit = (model.option == ExtendedMath)
       in
            Input.button (buttonStyleSelected width bit)
              { onPress = Just SelectStandard, label = el [] (Element.text "Standard")}


extendedMarkdownButton model width =
    let
            bit = (model.option == ExtendedMath)
    in
        Input.button (buttonStyleSelected width bit)
          { onPress = Just SelectExtended, label = el [] (Element.text "Extended")}


extendedMathMarkdownButton model width =
    let
        bit = (model.option == ExtendedMath)
    in
    Input.button (buttonStyleSelected width bit)
      { onPress = Just SelectExtendedMath, label = el [] (Element.text "ExtendedMath")}



buttonStyleSelected = buttonStyleSelected_ blue red

buttonStyleSelected_ : Color -> Color -> Int -> Bool -> List (Attr () msg)
buttonStyleSelected_ color color2 width_ bit =
    [ case bit of
        False -> Background.color color
        True -> Background.color color2

    , Font.color white
    , width (px width_)
    , height (px 25)
    , Font.size 9
    ]


blue = Element.rgb 0 0 255

red =  Element.rgb 200 0 0

white = Element.rgb 255 255 255