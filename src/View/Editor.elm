module View.Editor exposing (view, viewSubdocuments)

import Button
import CustomElement.CodeEditor as CodeEditor
import Document
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Html
import Html.Attributes as HA
import Model exposing (Model, Msg(..))
import Render exposing (RenderingOption(..))
import Render.Types exposing (RenderedText)
import Style
import Utility
import View.Common exposing (RenderedDocumentRecord, ViewInfo)
import View.Render
import View.Widget


view : ViewInfo -> Model -> Element Msg
view viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "---"

        rt : RenderedText Msg
        rt =
            Render.get model.renderingData

        newViewInfo =
            { viewInfo
                | docListWidth = viewInfo.docListWidth + 0.1 * viewInfo.editorWidth
                , editorWidth = viewInfo.editorWidth + viewInfo.tocWidth / 2 - 0.1 * viewInfo.editorWidth
                , renderedDisplayWidth = viewInfo.renderedDisplayWidth + viewInfo.tocWidth / 2
                , tocWidth = 0
            }
    in
    column []
        [ editingHeader newViewInfo model rt
        , row []
            [ View.Widget.tabStrip newViewInfo model
            , View.Widget.toolsOrDocs newViewInfo model
            , editor newViewInfo model
            , Element.Lazy.lazy (View.Render.renderedSource newViewInfo model footerText) rt
            ]
        , View.Widget.footer model
        ]


editingHeader : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
editingHeader viewInfo model rt =
    let
        lhWidth =
            View.Common.scale (viewInfo.toolStripWidth + viewInfo.docListWidth + viewInfo.editorWidth / 2) model.windowWidth

        rh =
            viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth

        --        titleWidth =
        --            scale (rh / 2) model.windowWidth
        titleWidth =
            View.Common.scale (0.45 * rh) model.windowWidth

        rhWidth =
            View.Common.scale (viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ View.Widget.modeButtonStrip model lhWidth
        , row [ spacing 10, width fill ]
            [ -- titleRowForEditing titleWidth rt
              View.Widget.searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]



-- EDITOR --


{-| A wrapper for editor\_, which does the real editing work.
-}
editor : ViewInfo -> Model -> Element Msg
editor viewInfo model =
    let
        w =
            View.Common.affine viewInfo.editorWidth viewInfo.hExtra model.windowWidth |> toFloat

        h =
            View.Common.translate -viewInfo.vInset model.windowHeight |> toFloat
    in
    column []
        [ Element.Keyed.el []
            ( String.fromInt 0
            , editor_ model w h
            )
        ]


{-| Does teh real editing work.
-}
editor_ : Model -> Float -> Float -> Element Msg
editor_ model w h =
    let
        wpx =
            Utility.pxFromFloat w

        hpx =
            Utility.pxFromFloat h
    in
    CodeEditor.codeEditor
        [ CodeEditor.editorValue (Document.getContent model.currentDocument)
        , CodeEditor.onEditorChanged UpdateDocumentText -- FeedDebouncer -- Inform the editor custom element of the change in text
        , CodeEditor.onGutterClicked ProcessLine -- Respond to clicks by scrolling the rendered to text to the corresponding position.
        ]
        []
        |> (\x -> Html.div [ View.Common.setHtmlId "_editor_", HA.style "width" wpx, HA.style "height" hpx, HA.style "overflow" "scroll" ] [ x ])
        |> Element.html



-- SUBDOCUMENT EDITOR


viewSubdocuments : ViewInfo -> Model -> Element Msg
viewSubdocuments viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "--"
    in
    column []
        [ simpleEditingHeader viewInfo model
        , row []
            [ View.Widget.tabStrip viewInfo model
            , View.Widget.toolsOrDocs viewInfo model
            , subDocumentTools model
            , column [ spacing 12, alignTop, padding 20 ]
                [ row [ spacing 8 ] [ el [ Font.size 14 ] (Element.text "Edit outline below"), Button.setupOutline model, Button.updateChildren model ]
                , inputOutline model
                ]
            ]
        , View.Widget.footer model
        ]


simpleEditingHeader : ViewInfo -> Model -> Element Msg
simpleEditingHeader viewInfo model =
    let
        lhWidth =
            View.Common.scale (viewInfo.toolStripWidth + viewInfo.docListWidth + viewInfo.editorWidth / 2) model.windowWidth

        rh =
            viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth

        --        titleWidth =
        --            scale (rh / 2) model.windowWidth
        titleWidth =
            View.Common.scale (0.45 * rh) model.windowWidth

        rhWidth =
            View.Common.scale (viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ View.Widget.modeButtonStrip model lhWidth
        , row [ spacing 10, width fill ]
            [ el [ Font.color Style.white ] (Element.text "Subdocument Editor")
            , View.Widget.searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]


subDocumentTools model =
    let
        ( message1, message2 ) =
            case model.currentDocument of
                Nothing ->
                    ( "Master document not selected", "" )

                Just master ->
                    ( "Search, then click below to add subdocument to", master.title )
    in
    column [ spacing 12, paddingXY 18 24, alignTop ]
        [ el [ Font.size 14, width (px 300) ] (Element.text message1)
        , el [ Font.size 14, width (px 300), Font.bold ] (Element.text message2)
        , column [ Font.size 13, spacing 8, width (px 350), height (px 500), Border.color Style.charcoal, Border.width 1, padding 12, scrollbarY ]
            (List.map Button.addSubdocument2 model.candidateChildDocumentList)
        ]


inputOutline model =
    Input.multiline (Style.textInputStyleSimple 300 500)
        { onChange = GotOutline
        , text = model.documentOutline
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "")
        , spellcheck = False
        }
