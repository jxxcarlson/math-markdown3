module View.Reader exposing (view)

import Document
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Model exposing (Model, Msg(..))
import Render exposing (RenderingOption(..))
import Render.Types exposing (RenderedText)
import Style
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
    in
    column [ paddingXY 0 0 ]
        [ readingHeader viewInfo model rt
        , row []
            [ View.Widget.tabStrip viewInfo model
            , View.Widget.toolsOrDocs viewInfo model
            , Element.Lazy.lazy (View.Render.renderedSource viewInfo model footerText) rt
            ]
        , View.Widget.footer model
        ]


readingHeader : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
readingHeader viewInfo model rt =
    let
        lhWidth =
            View.Common.scale (viewInfo.toolStripWidth + viewInfo.docListWidth) model.windowWidth

        -- scale viewInfo.docListWidth model.windowWidth
        titleWidth =
            View.Common.scale (0.75 * viewInfo.renderedDisplayWidth) model.windowWidth

        rhWidth =
            View.Common.scale (0.25 * viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ View.Widget.modeButtonStrip model lhWidth
        , row [ spacing 10, alignLeft ]
            [ titleRow titleWidth rt
            , View.Widget.searchRow model
            ]
        ]


titleRow titleWidth rt =
    row [ Font.size 24, height (px 40), width (px titleWidth), Font.color Style.white, alignRight, clipX ]
        [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ]
