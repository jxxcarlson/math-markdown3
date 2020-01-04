module View.Render exposing (renderedSource, renderedSourceForEditing)

import Cmd.Document
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (pre)
import Html.Attributes as HA
import Model
    exposing
        ( --                  AppMode(..)
          --                 , DequeViewState(..)
          --                 , DocumentDeleteState(..)
          --                 , DocumentListDisplay
          --                 , DocumentListType(..)
          --                 , EditMode(..)
          --                 , FocusedElement(..)
          --                 , Message
          --                 , MessageType(..)
          Model
        , Msg(..)
          --                 , SearchMode(..)
          --                 , SearchType(..)
          --                 , SortMode(..)
          --                 , UserState(..)
          --                 , Visibility(..)
        )
import Render.Types exposing (RenderedText)
import Style
import View.Common exposing (ViewInfo)


renderedSource : ViewInfo -> Model -> String -> RenderedText Msg -> Element Msg
renderedSource viewInfo model footerText_ rt =
    let
        w_ =
            View.Common.affine viewInfo.renderedDisplayWidth viewInfo.hExtra model.windowWidth

        h_ =
            View.Common.translate -viewInfo.vInset model.windowHeight

        w2_ =
            View.Common.affine viewInfo.renderedDisplayWidth (viewInfo.hExtra + 160) model.windowWidth

        wToc =
            View.Common.affine viewInfo.tocWidth viewInfo.hExtra model.windowWidth

        hToc =
            View.Common.translate -viewInfo.vInset model.windowHeight

        outerSourceStyle =
            [ View.Common.setElementId "__rt_scroll__", width (px w_), height (px h_), Element.paddingEach { left = 0, right = 20, top = 0, bottom = 30 }, clipX, Font.size 12 ]

        innerSourceStyle =
            [ View.Common.setElementId Cmd.Document.masterId, height (px h_) ]

        outerTocStyle =
            [ height (px hToc), width (px wToc), Font.size 12, paddingXY 8 0, Background.color (Style.makeGrey 0.9) ]

        innerTocStyle =
            [ height (px (hToc - 125)), scrollbarY, clipX ]

        footerStyle =
            [ paddingXY 12 3, width fill, height (px 125), clipX, Background.color (Style.makeGrey 0.5), Font.color (Style.makeGrey 1.0) ]
    in
    row [ spacing 10 ]
        [ column outerSourceStyle
            [ column [ width (px w2_), paddingXY 10 20 ]
                [ column innerSourceStyle [ rt.document |> Element.html ] ]
            ]
        , Element.column outerTocStyle
            [ column innerTocStyle [ rt.toc |> Element.html ]
            , column footerStyle
                [ renderFooter footerText_ ]
            ]
        ]


renderedSourceForEditing : ViewInfo -> Model -> String -> RenderedText Msg -> Element Msg
renderedSourceForEditing viewInfo model footerText_ rt =
    let
        w_ =
            View.Common.affine viewInfo.renderedDisplayWidth viewInfo.hExtra model.windowWidth

        h_ =
            View.Common.translate -viewInfo.vInset model.windowHeight

        w2_ =
            View.Common.affine viewInfo.renderedDisplayWidth (viewInfo.hExtra + 160) model.windowWidth

        wToc =
            View.Common.affine viewInfo.tocWidth viewInfo.hExtra model.windowWidth

        hToc =
            View.Common.translate -viewInfo.vInset model.windowHeight

        outerSourceStyle =
            [ View.Common.setElementId "__rt_scroll__", width (px w_), height (px h_), Element.paddingEach { left = 0, right = 20, top = 0, bottom = 30 }, clipX, Font.size 12 ]

        innerSourceStyle =
            [ View.Common.setElementId Cmd.Document.masterId, height (px h_) ]

        outerTocStyle =
            [ height (px hToc), width (px wToc), Font.size 12, paddingXY 8 0, Background.color (Style.makeGrey 0.9) ]

        innerTocStyle =
            [ height (px (hToc - 125)), scrollbarY, clipX ]

        footerStyle =
            [ paddingXY 12 3, width fill, height (px 125), clipX, Background.color (Style.makeGrey 0.5), Font.color (Style.makeGrey 1.0) ]
    in
    row [ spacing 10 ]
        [ column outerSourceStyle
            [ column [ width (px w2_), paddingXY 10 20 ]
                [ column innerSourceStyle [ rt.document |> Element.html ] ]
            ]

        --        , Element.column outerTocStyle
        --            [ column innerTocStyle [ rt.toc |> Element.html ]
        --            , column footerStyle
        --                [ renderFooter footerText_ ]
        --            ]
        ]


renderFooter : String -> Element Msg
renderFooter str =
    pre [ HA.style "font-size" "10px" ] [ Html.text str ] |> Element.html
