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
    , windowWidth : Int
    , windowHeight : Int
    }



init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { sourceText = Strings.initialText
            , counter = 0
            , seed = 0
            , option = ExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            }
    in
    ( model, Cmd.none )

-- MSG --

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
    {  width : Int
      , height : Int
    }


viewInfo = {
    left = 0.38
  , middle = 0.38
  , right = 0.24
  , vInset = 210.0
  , hExtra = 20.0
  }

scale : Float -> Int -> Int
scale factor input =
    factor * (toFloat input) |> round

affine : Float -> Float -> Int -> Int
affine factor shift input =
    factor * ((toFloat input) - shift) |> round

translate : Float -> Int -> Int
translate amount input =
    (toFloat input)  + amount |> round

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
  let
     left =  scale viewInfo.left model.windowWidth
     middle =   scale viewInfo.middle model.windowWidth
     right = scale viewInfo.right model.windowWidth
  in
    row [ height (px 45), width (px model.windowWidth), Background.color charcoal, paddingXY 30 0] [
      column [width (px left)] []
     , column [width (px middle), Font.size 12, Font.color white] [rt.title |> Element.html]
     , column [width (px right)] []
    ]

footer : Model -> Element Msg
footer model =
   let
        left =  scale viewInfo.left model.windowWidth
        middle =   scale viewInfo.middle model.windowWidth
        right = scale viewInfo.right model.windowWidth
     in
       row [ height (px 30), width (px model.windowWidth), Background.color charcoal, paddingXY 30 0] [
         column [width (px left)] [row [centerX, spacing 10] [clearButton 60, restoreButton 60 ]]
        , column [width (px middle), Font.size 12, Font.color white] [flavors model]
        , column [width (px right)] [status model]
       ]

status model =
  el [Font.color white, Font.size 12, centerX]
     (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)

flavors model =
   row [spacing 10,  centerX] [
       el [Font.color white, Font.bold] (Element.text "Markdown Flavor")
      ,  standardMarkdownButton model 80
      , extendedMarkdownButton model 80
      , extendedMathMarkdownButton model 93 ]


editor : Model -> Element Msg
editor model =
   let
       w_ = affine viewInfo.left (viewInfo.hExtra) model.windowWidth |> toFloat
       h_ = translate (-viewInfo.vInset) model.windowHeight |> toFloat

   in
    column []
            [ Element.Keyed.el []
                ( String.fromInt model.counter
                , Input.multiline (textInputStyle w_ h_)
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

        w_ = affine viewInfo.middle (viewInfo.hExtra) model.windowWidth
        h_ = translate (-viewInfo.vInset) model.windowHeight

        w2_ = affine viewInfo.middle (viewInfo.hExtra + 140) model.windowWidth

        wToc = affine viewInfo.right (viewInfo.hExtra) model.windowWidth
        hToc = translate (-viewInfo.vInset) model.windowHeight
    in
      row [spacing 10] [
         Element.Keyed.column [width (px w_), height (px h_), scrollbarY, clipX, Font.size 12]
           [ ( token, column [width (px w2_), paddingXY 10 20 ] [rt.document |> Element.html] ) ]
        , Element.column [width (px wToc), height (px hToc), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (makeGrey 0.9)]
                                    [ rt.toc |> Element.html  ]
      ]


lightGrey =
    makeGrey 0.95


---- BUTTONS --


clearButton width =
    Input.button (buttonStyleSelected width False)
                  { onPress = Just Clear, label = el [centerX] (Element.text "Clear")}

restoreButton width =
    Input.button (buttonStyleSelected width False)
                  { onPress = Just RestoreText, label = el [centerX] (Element.text "Restore")}




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

textInputStyle w h=
    [ preWrap
    , height <| px <| round h
    , width <| px <| round w
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