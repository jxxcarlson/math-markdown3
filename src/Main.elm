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
import Request exposing(RequestMsg(..))
import RemoteData exposing (RemoteData(..))
import Graphql.Http.GraphqlError

main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


type alias Model =
    { counter : Int
    , seed : Int
    , option : Option
    , windowWidth : Int
    , windowHeight : Int
    , visibilityOfTools : Visibility
    , appMode: AppMode
    , zone : Time.Zone
    , time : Time.Posix
    , currentUser : Maybe User
    , message : String
    -- documents
    , documentList : List Document
    , currentDocument : Maybe Document
    }

type Visibility = Visible | Invisible

type AppMode = Reading | Editing

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { counter = 0
            , seed = 0
            , option = ExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , visibilityOfTools = Invisible
            , appMode = Reading
            , zone = Time.utc
            , time = Time.millisToPosix 0
            , currentUser = Just User.dummy
            , message = "Starting ..."
            -- documents
            , documentList = [Data.startupDocument, Data.doc2, Data.doc3]
            , currentDocument = Just Data.startupDocument
            }
    in
    ( model, Task.perform AdjustTimeZone Time.here )

-- MSG --

type Msg
    = NoOp
    -- Time
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    -- Random
    | GenerateSeed
    | NewSeed Int
    -- UI
    | SelectStandard
    | SelectExtended
    | SelectExtendedMath
    | SetToolPanelState Visibility
    | SetAppMode AppMode
    | WindowSize Int Int
    -- Document
    | CreateDocument
    | SaveDocument
    | GetUserDocuments
    | UpdateDocumentText String
    | SetCurrentDocument Document
    | Clear
    | Req RequestMsg





type alias Flags =
    {  width : Int
      , height : Int
    }




type alias ViewInfo = {
      toolStripWidth : Float
    , docListWidth : Float
    , editorWidth : Float
    , renderedDisplayWidth : Float
    , tocWidth : Float
    , vInset : Float
    , hExtra: Float

  }

vInset = 75 -- 208

viewInfoEditing = {
    toolStripWidth = 0.05
  , docListWidth = 0.15
  , editorWidth = 0.3
  , renderedDisplayWidth = 0.3
  , tocWidth = 0.2
  , vInset = vInset
  , hExtra = 0
  }

viewInfoReading = {
    toolStripWidth = 0.05
  ,  docListWidth = 0.2
  , editorWidth = 0
  , renderedDisplayWidth = 0.45
  , tocWidth = 0.3
  , vInset = vInset
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
    Sub.batch [
        Time.every 1000 Tick
        -- , WindowSize flags.width, flags.height
      ]


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
                | currentDocument  = Maybe.map (Document.setContent model.time "") model.currentDocument
                , counter = model.counter + 1
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


        SetToolPanelState visibility ->
            ( {model | visibilityOfTools = visibility}, Cmd.none)


        SetAppMode appMode ->
            case appMode of
                Reading ->  ( {model | appMode = Reading}, Cmd.none)
                Editing ->  ( {model | appMode =  Editing}, Cmd.none)

        -- TIME --

        Tick newTime ->
              ( { model | time = newTime }
              , Cmd.none
              )

        WindowSize width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

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
                     , Request.createDocument newDocument |> Cmd.map Req
                   )


        SaveDocument -> (model, Cmd.none)

        GetUserDocuments ->
            case model.currentUser of
                Nothing -> ({model | message = "Can't get documents if user is not signed in"}, Cmd.none)
                Just user ->
                    ({model | message = "Getting your documents"}, Request.documentsByAuthor user.id |> Cmd.map Req)

        UpdateDocumentText str ->
            case model.currentDocument of
                Nothing -> (model, Cmd.none)
                Just doc ->
                   let
                     updatedDoc = Document.setContent model.time str doc
                   in
                     ( { model | currentDocument = Just updatedDoc
                               , documentList = Document.replaceInList updatedDoc model.documentList
                       }
                       ,
                       Cmd.none
                     )

        SetCurrentDocument document ->
            ( {model | currentDocument = Just document}, Cmd.none)


        Req requestMsg ->
            case requestMsg of
              GotNewDocument remoteData ->
                case remoteData of
                   NotAsked -> ({ model | message = "New doc: not asked"} , Cmd.none)
                   Loading -> ({model | message = "Mew doc: loading"} , Cmd.none)
                   Failure _ -> ({model | message = "New doc: failed request"} , Cmd.none)
                   Success _ -> ({model | message = "New document created"} , Cmd.none)
              GotUserDocuments remoteData  ->
                case remoteData of
                   NotAsked -> ({ model | message = "Get author docs: not asked"} , Cmd.none)
                   Loading -> ({model | message = "Get author docs:: loading"} , Cmd.none)
                   Failure _ -> ({model | message = "Get author docs:: request failed"} , Cmd.none)
                   Success maybeDocumentList ->
                    case maybeDocumentList of
                      Nothing -> ({model |documentList = [], currentDocument = Nothing,  message = "No documents retuned for author"} , Cmd.none)
                      Just documentList -> ({model | documentList = documentList
                                                , currentDocument = List.head documentList
                                                , message = "Success returning documennt list for authoor!"} , Cmd.none)

          -- MANAGE DOCUMENTS --



--
-- VIEW FUNCTIONS
---

type alias RenderedDocumentRecord msg = { document : Html msg, title : Html msg, toc : Html msg }

view : Model -> Html Msg
view model =
    case model.appMode of
        Reading ->  Element.layout []  (readingDisplay viewInfoReading model)
        Editing ->  Element.layout []  (editingDisplay viewInfoEditing model)


editingDisplay : ViewInfo -> Model -> Element Msg
editingDisplay viewInfo model =
  let
      rt : {title: Html msg, toc: Html msg, document: Html msg}
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option (Document.getContent model.currentDocument)
  in
    column [ paddingXY 0 0]
        [
           header viewInfo model rt
         , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, editor viewInfo model, renderedSource viewInfo model rt ]
         , footer model
         ]

readingDisplay : ViewInfo -> Model -> Element Msg
readingDisplay viewInfo model =
  let
      footerText = Maybe.map Document.footer model.currentDocument |> Maybe.withDefault ""
      rt : {title: Html msg, toc: Html msg, document: Html msg}
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option ((Document.getContent model.currentDocument) ++ footerText )
  in
    column [ paddingXY 0 0]
        [
           header viewInfo model rt
         , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, renderedSource viewInfo  model rt ]
         , footer model
         ]

toolsOrDocs viewInfo model =
    case model.visibilityOfTools of
        Visible ->  toolPanel viewInfo model
        Invisible -> docListViewer viewInfo model

-- DOCUMENT VIEWS (EDITOR, RENDERED, TOC) --

-- EDITOR --

editor : ViewInfo -> Model -> Element Msg
editor viewInfo model =
   let
       w_ = affine viewInfo.editorWidth (viewInfo.hExtra) model.windowWidth |> toFloat
       h_ = translate (-viewInfo.vInset) model.windowHeight |> toFloat

   in
    column []
            [ Element.Keyed.el []
                ( String.fromInt model.counter
                , Input.multiline (textInputStyle w_ h_)
                    { onChange = UpdateDocumentText
                    , text = Document.getContent model.currentDocument
                    , placeholder = Nothing
                    , label = Input.labelBelow [ Font.size 0, Font.bold ] (Element.text "")
                    , spellcheck = False
                    }
                )
            ]


editingModeButton model =
  let
      color = if model.appMode == Editing then
           red
        else
           blue
  in
    Input.button [] { onPress = Just (SetAppMode Editing)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12]
            (Element.text "Edit")}


readingModeButton model =
  let
      color = if model.appMode == Reading then
           red
        else
           blue
  in
    Input.button [] { onPress = Just (SetAppMode Reading)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12]
       (Element.text "Read")}


-- RENDERED SOURCE --

renderedSource : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element msg
renderedSource viewInfo model rt =
    let
        token =
            String.fromInt model.counter

        w_ = affine viewInfo.renderedDisplayWidth (viewInfo.hExtra) model.windowWidth
        h_ = translate (-viewInfo.vInset) model.windowHeight

        w2_ = affine viewInfo.renderedDisplayWidth (viewInfo.hExtra + 160) model.windowWidth

        wToc = affine viewInfo.tocWidth (viewInfo.hExtra) model.windowWidth
        hToc = translate (-viewInfo.vInset) model.windowHeight
    in
      row [spacing 10] [
         Element.Keyed.column [width (px w_), height (px h_),               clipX, Font.size 12]
           [ ( token, column [width (px w2_), paddingXY 10 20 ] [rt.document |> Element.html] ) ]
        , Element.column [width (px wToc), height (px hToc), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (makeGrey 0.9)]
                                    [ rt.toc |> Element.html  ]
      ]

-- TOOL PANEL --

toolPanel viewInfo model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
      heading_ = el [Font.size 16, Font.bold] (Element.text "Report bugs!  ")
  in
    column [width (px (scale viewInfo.docListWidth model.windowWidth)), height (px h_), Background.color (makeGrey 0.5)
       , paddingXY 20 20, alignTop]
      [column [Font.size 13, spacing 15]  [
          el [Font.size 16, Font.bold, Font.color white] (Element.text "Tools")
        , getUserDocumentsButton
        , newDocumentButton
        , saveDocumentButton
        , flavors model
       ]
  ]


toolButtonStyle : List (Element.Attribute msg)
toolButtonStyle = [height (px 30), width (px 150),  padding 8, Background.color charcoal, Font.color white, Font.size 12]

newDocumentButton =
        Input.button [] { onPress = Just (CreateDocument)
                , label = el toolButtonStyle (Element.text "Create")}

saveDocumentButton =
        Input.button [] { onPress = Just (SaveDocument)
                , label = el toolButtonStyle (Element.text "Save")}

getUserDocumentsButton =
        Input.button [] { onPress = Just (GetUserDocuments)
                , label = el toolButtonStyle (Element.text "Get Documents")}

-- DOCUMENT LIST --

docListViewer viewInfo model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
  in
    column [width (px (scale viewInfo.docListWidth model.windowWidth)), height (px h_), Background.color (makeGrey 0.9)
       , paddingXY 12 20, alignTop, clipX]
      [column [Font.size 13, spacing 8]  (heading::(List.map (tocEntry model.currentDocument) model.documentList))]


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        color = case currentDocument_ of
            Nothing ->  blue
            Just currentDocument ->
                if currentDocument.identifier == document.identifier then
                   red
                else
                   blue
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [Font.color color] (Element.text document.title)}


heading = el [Font.size 16, Font.bold] (Element.text "Documents")


-- HEADER AND FOOTER --

header : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
header viewInfo model rt =
  let
     editorWidth_ =  scale viewInfo.editorWidth model.windowWidth
     renderedDisplayWidth_ =   scale viewInfo.renderedDisplayWidth model.windowWidth
     innerTOCWidth_ = scale viewInfo.tocWidth model.windowWidth
  in
    row [ height (px 45), width (px (model.windowWidth)), Background.color charcoal] [
      row [width (px editorWidth_ ), spacing 10, paddingXY 30 0] [ editingModeButton model, readingModeButton model]
     , column [width (px renderedDisplayWidth_), Font.size 12, Font.color white, alignRight, moveUp 8] [rt.title |> Element.html |> Element.map (\_ -> NoOp)]
     , column [width (px innerTOCWidth_)] []
    ]

-- TAB-STRIP ON LEFT --

tabStrip : ViewInfo -> { a | visibilityOfTools : Visibility } -> Element Msg
tabStrip viewInfo model =
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
        editorWidth_  =  scale viewInfoEditing.editorWidth  model.windowWidth
        renderedDisplayWidth =   scale viewInfoEditing.renderedDisplayWidth model.windowWidth
        innerTOCWidth_ = scale viewInfoEditing.tocWidth model.windowWidth
     in
       row [ height (px 30), width (px (model.windowWidth)), Background.color charcoal] [
         row [width (px editorWidth_ )] [row [centerX, spacing 25] [currentAuthorDisplay model, wordCount model ]]
        , row [width (px renderedDisplayWidth), Font.size 12, Font.color white] [el [] (Element.text <| model.message) ]
        , row [width (px innerTOCWidth_), spacing 25] [currentTime model, status model]
       ]


currentAuthorDisplay model =
    let
       message = case model.currentUser of
          Nothing -> "Not signed in"
          Just user -> "Signed in as " ++ user.id
     in
      Element.el [Font.color white, Font.size 12]
           (Element.text <| message )



currentTime model =
      Element.el [Font.color white, Font.size 12]
        (Element.text <| "Current time: " ++ Utility.humanTimeHM model.zone model.time)


wordCount model =
    let
         sourceText = case model.currentDocument of
            Just document ->
                document.content
            _ -> ""
         wc =  Utility.wordCount sourceText

    in
      Element.el [Font.color white, Font.size 12] (Element.text <| "Word count: " ++ wc)


status model =
  el [Font.color white, Font.size 12, centerX]
     (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)

flavors model =
   column [spacing 10, Background.color charcoal, padding 22] [
       el [Font.color white, Font.bold] (Element.text "Markdown Flavor")
      ,  standardMarkdownButton model 93
      , extendedMarkdownButton model 93
      , extendedMathMarkdownButton model 93 ]




---- BUTTONS --



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