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
import Data
import Document exposing(Document)
import Time
import Task
import Utility exposing (humanTimeHM)
import User exposing(User)

main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model =
    {  counter : Int
    , seed : Int
    , option : Option
    , windowWidth : Int
    , windowHeight : Int
    , documentList : List Document
    , currentDocument : Maybe Document
    , visibilityOfTools : Visibility
    , zone : Time.Zone
    , time : Time.Posix
    , currentUser : Maybe User
    }

type Visibility = Visible | Invisible

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { counter = 0
            , seed = 0
            , option = ExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , documentList = [Data.startupDocument, Data.doc2]
            , currentDocument = Just Data.startupDocument
            , visibilityOfTools = Invisible
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , currentUser = Just User.dummy

            }
    in
    ( model, Task.perform AdjustTimeZone Time.here )

-- MSG --

type Msg
    = Clear
    | NoOp
    | GetContent String
    | GenerateSeed
    | NewSeed Int
    | RestoreText
    | RefreshText
    | SelectStandard
    | SelectExtended
    | SelectExtendedMath
    | InputNotes String
    | SetCurrentDocument Document
    | SetToolPanelState Visibility
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    -- Document
    | CreateDocument


type alias Flags =
    {  width : Int
      , height : Int
    }


viewInfo = {
    toolStrip = 0.05
  ,  toc = 0.15
  , left = 0.3
  , middle = 0.3
  , right = 0.2
  , vInset = 210.0
  , hExtra = 0
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
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear ->
            ( { model
                | currentDocument  = Maybe.map (Document.setContent "") model.currentDocument
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        RestoreText ->
            ( { model
                | counter = model.counter + 1
                , currentDocument = Just Data.startupDocument
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

           ( {model | currentDocument = Maybe.map (Document.setContent str) model.currentDocument }, Cmd.none )

        SetCurrentDocument document ->
            ( {model | currentDocument = Just document}, Cmd.none)

        SetToolPanelState visibility ->
            ( {model | visibilityOfTools = visibility}, Cmd.none)

        -- TIME --

        Tick newTime ->
              ( { model | time = newTime }
              , Cmd.none
              )

        AdjustTimeZone newZone ->
              ( { model | zone = newZone }
              , Cmd.none
              )

        -- DOCUMENT --

        CreateDocument ->
           case model.currentUser of
               Nothing -> (model, Cmd.none)
               Just user ->
                   let
                      newDocument = Document.create user.id "New Document" model.time "# New Document\n\nWrite something here ..."
                   in
                   ({model | currentDocument = Just newDocument
                            , documentList = newDocument :: model.documentList
                            , visibilityOfTools = Invisible
                     }
                     , Cmd.none
                   )

        GetContent str ->
            case model.currentDocument of
                Nothing -> (model, Cmd.none)
                Just doc ->
                    let
                        updatedDoc =  Document.setContent str doc
                    in
                    ( { model
                        | currentDocument = Just <| updatedDoc
                        --, documentList = Document.replaceInList updatedDoc model.documentList
                        , counter = model.counter + 1
                      }
                    , Cmd.none
                    )

-- MANAGE DOCUMENTS --







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
      rt : {title: Html msg, toc: Html msg, document: Html msg}
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option (Document.getContent model.currentDocument)
  in
    column [ paddingXY 0 0]
        [
           header model rt
         , row [] [ tabStrip model, toolsOrDocs model, editor model, renderedSource model rt ]
         , footer model
         ]


toolsOrDocs model =
    case model.visibilityOfTools of
        Visible ->  toolPanel model
        Invisible -> docListViewer model

-- DOCUMENT VIEWS (EDITOR, RENDERED, TOC) --

-- EDITOR --

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
                    , text = Document.getContent model.currentDocument
                    , placeholder = Nothing
                    , label = Input.labelBelow [ Font.size 0, Font.bold ] (Element.text "")
                    , spellcheck = False
                    }
                )
            ]


-- RENDERED SOURCE --

renderedSource : Model -> RenderedDocumentRecord msg -> Element msg
renderedSource model rt =
    let
        token =
            String.fromInt model.counter

        w_ = affine viewInfo.middle (viewInfo.hExtra) model.windowWidth
        h_ = translate (-viewInfo.vInset) model.windowHeight

        w2_ = affine viewInfo.middle (viewInfo.hExtra + 160) model.windowWidth

        wToc = affine viewInfo.right (viewInfo.hExtra) model.windowWidth
        hToc = translate (-viewInfo.vInset) model.windowHeight
    in
      row [spacing 10] [
         Element.Keyed.column [width (px w_), height (px h_), scrollbarY, clipX, Font.size 12]
           [ ( token, column [width (px w2_), paddingXY 10 20 ] [rt.document |> Element.html] ) ]
        , Element.column [width (px wToc), height (px hToc), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (makeGrey 0.9)]
                                    [ rt.toc |> Element.html  ]
      ]

-- TOOL PANEL --

toolPanel model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
      heading_ = el [Font.size 16, Font.bold] (Element.text "Report bugs!  ")
  in
    column [width (px (scale viewInfo.toc model.windowWidth)), height (px h_), Background.color (makeGrey 0.5)
       , paddingXY 10 20, alignTop]
      [column [Font.size 13, spacing 8]  [
          el [Font.size 16, Font.bold, Font.color white] (Element.text "Tools")
        , newDocumentButton
       ]
  ]


newDocumentButton =
        Input.button [] { onPress = Just (CreateDocument)
                , label = el [height (px 30), width (px 150),  padding 8, Background.color charcoal, Font.color white, Font.size 12] (Element.text "Create document")}

-- DOCUMENT LIST --

docListViewer model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
  in
    column [width (px (scale viewInfo.toc model.windowWidth)), height (px h_), Background.color (makeGrey 0.9)
       , paddingXY 10 20, alignTop]
      [column [Font.size 13, spacing 8]  (heading::(List.map (tocEntry model.currentDocument) model.documentList))]


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        color = case currentDocument_ of
            Nothing ->  blue
            Just currentDocument ->
                if currentDocument.id == document.id then
                   red
                else
                   blue
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [Font.color color] (Element.text document.title)}


heading = el [Font.size 16, Font.bold] (Element.text "Documents")


-- HEADER AND FOOTER --

header : Model -> RenderedDocumentRecord msg -> Element Msg
header model rt =
  let
     left =  scale viewInfo.left model.windowWidth
     middle =   scale viewInfo.middle model.windowWidth
     right = scale viewInfo.right model.windowWidth
  in
    row [ height (px 45), width (px model.windowWidth), Background.color charcoal, paddingXY 30 0] [
      column [width (px left)] []
     , column [width (px middle), Font.size 12, Font.color white] [rt.title |> Element.html |> Element.map (\_ -> NoOp)]
     , column [width (px right)] []
    ]

-- TAB-STRIP ON LEFT --

tabStrip : { a | visibilityOfTools : Visibility } -> Element Msg
tabStrip model =
    column [width (px 30), height(px 200), Background.color (grey 0.1), alignTop ] [
        row [spacing 15, rotate -1.5708,moveLeft 50, moveDown 70] [showToolsButton model, showDocumentListButton model]
    ]




showToolsButton : { a | visibilityOfTools : Visibility } -> Element Msg
showToolsButton model =
  let
      color = if model.visibilityOfTools == Visible then
           red
        else
           blue
  in
    Input.button [] { onPress = Just (SetToolPanelState Visible)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12] (Element.text "Tools")}


showDocumentListButton : { a | visibilityOfTools : Visibility } -> Element Msg
showDocumentListButton model =
  let
      color = if model.visibilityOfTools == Invisible then
           red
        else
           blue
  in
   Input.button [ ] { onPress = Just (SetToolPanelState Invisible)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12] (Element.text "Documents")}



-- FOOTER --

footer : Model -> Element Msg
footer model =
   let
        left =  scale viewInfo.left model.windowWidth
        middle =   scale viewInfo.middle model.windowWidth
        right = scale viewInfo.right model.windowWidth
     in
       row [ height (px 30), width (px model.windowWidth), Background.color charcoal, paddingXY 30 0] [
         row [width (px left)] [row [centerX, spacing 10] [wordCount model ]]
        , row [width (px middle), Font.size 12, Font.color white] [flavors model]
        , row [width (px right)] [currentTime model, status model]
       ]


-- currentAuthorDixpay


currentTime model =
      Element.el [Font.color white, Font.size 12]
        (Element.text <| "Current time: " ++ Utility.humanTimeHM model.zone model.time)


wordCount model =
    let
         sourceText = case model.currentDocument of
            Just document ->
                document.content
            _ -> ""
         wc =  List.length (String.words sourceText) |> String.fromInt

    in
      Element.el [Font.color white, Font.size 12] (Element.text <| "Word count: " ++ wc)


status model =
  el [Font.color white, Font.size 12, centerX]
     (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)

flavors model =
   row [spacing 10,  centerX] [
       el [Font.color white, Font.bold] (Element.text "Markdown Flavor")
      ,  standardMarkdownButton model 80
      , extendedMarkdownButton model 80
      , extendedMathMarkdownButton model 93 ]




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


-- COLORS --

makeGrey g =
    Element.rgb g g g


lightGrey =
    makeGrey 0.95

blue = grey 0.5

red =  Element.rgb 0.4 0.1 0.1

white = Element.rgb 1 1 1

grey g = Element.rgb g g g

charcoal = grey 0.3

black = grey 0