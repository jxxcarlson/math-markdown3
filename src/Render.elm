module Render exposing (RenderingData(..), RenderingOption(..), documentOption, get, load, loadFast, render, update)

import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Html exposing (Html)
import Html.Attributes as HA
import Markdown.ElmWithId
import Markdown.Option as MDOption
import MiniLatex
import MiniLatex.Edit
import MiniLatex.Render exposing (MathJaxRenderOption(..))
import ParseWithId
import Render.Markdown
import Render.Types exposing (RenderedText)
import Tree exposing (Tree)


{-|

    MD =
        Markdown

    ML =
        MiniLatex

-}
type RenderingData msg
    = MD (MDData msg)
    | ML (MLData msg)


type alias MLData msg =
    { fullText : Maybe String
    , editRecord : MiniLatex.Edit.Data (Html msg)
    }


type RenderingOption
    = OMarkdown MDOption.Option
    | OMiniLatex


documentOption : Document -> RenderingOption
documentOption doc =
    case doc.docType of
        Markdown flavor ->
            case flavor of
                MDStandard ->
                    OMarkdown MDOption.Standard

                MDExtended ->
                    OMarkdown MDOption.Extended

                MDExtendedMath ->
                    OMarkdown MDOption.ExtendedMath

        MiniLaTeX ->
            OMiniLatex


type alias MDData msg =
    { option : MDOption.Option
    , renderedText : RenderedText msg
    , initialAst : Tree ParseWithId.MDBlockWithId
    , fullAst : Tree ParseWithId.MDBlockWithId
    }


load : Int -> RenderingOption -> String -> RenderingData msg
load counter option source =
    case option of
        OMarkdown opt ->
            loadMarkdown counter opt source

        OMiniLatex ->
            loadMiniLatex counter source


loadFast : Int -> RenderingOption -> String -> RenderingData msg
loadFast counter option source =
    case option of
        OMarkdown opt ->
            loadMarkdownFast counter opt source

        OMiniLatex ->
            loadMiniLatexFast counter source


render : RenderingData msg -> RenderingData msg
render rd =
    case rd of
        MD data ->
            MD { data | renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" data.fullAst }

        ML data ->
            case data.fullText of
                Nothing ->
                    ML data

                Just fullText ->
                    loadMiniLatex -2 fullText


update : Int -> String -> RenderingData msg -> RenderingData msg
update version source rd =
    case rd of
        MD data ->
            let
                newAst =
                    Render.Markdown.diffUpdateAst data.option version source data.fullAst
            in
            MD { data | fullAst = newAst, renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" newAst }

        ML data ->
            ML { data | editRecord = MiniLatex.Edit.update NoDelay version source data.editRecord }


get : RenderingData msg -> RenderedText msg
get rd =
    case rd of
        MD data ->
            data.renderedText

        ML data ->
            { document = MiniLatex.Edit.get data.editRecord |> Html.div []
            , title = Html.span [ HA.style "font-size" "24px" ] [ Html.text (getTitle data.editRecord) ]
            , toc = innerTableOfContents data.editRecord.latexState
            }


getTitle : MiniLatex.Edit.Data (Html msg) -> String
getTitle data =
    data.latexState.tableOfContents
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "TITLE"



{- HIDDEN, MARKDOWN -}


loadMarkdown : Int -> MDOption.Option -> String -> RenderingData msg
loadMarkdown counter option str =
    let
        ast =
            Markdown.ElmWithId.parse counter MDOption.ExtendedMath str
    in
    MD
        { option = option
        , initialAst = ast
        , fullAst = ast
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" ast
        }


loadMarkdownFast : Int -> MDOption.Option -> String -> RenderingData msg
loadMarkdownFast counter option str =
    let
        fullAst =
            Markdown.ElmWithId.parse (counter + 1) MDOption.ExtendedMath str

        initialAst =
            Markdown.ElmWithId.parse counter option (getFirstPart str)
    in
    MD
        { option = option
        , initialAst = initialAst
        , fullAst = fullAst
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" initialAst
        }



{- HIDDEN, MINILATEX -}


loadMiniLatex : Int -> String -> RenderingData msg
loadMiniLatex version str =
    ML { fullText = Nothing, editRecord = MiniLatex.Edit.init Delay version str }


loadMiniLatexFast : Int -> String -> RenderingData msg
loadMiniLatexFast version str =
    ML { fullText = Just str, editRecord = MiniLatex.Edit.init Delay version (getFirstPart str) }



--    let
--        fullAst =
--            Markdown.ElmWithId.parse (counter + 1) MDOption.ExtendedMath str
--
--        initialAst =
--            Markdown.ElmWithId.parse counter option (getFirstPart str)
--    in
--    MD
--        { option = option
--        , initialAst = initialAst
--        , fullAst = fullAst
--        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" initialAst
--        }


{-| HELPERS
-}
getFirstPart : String -> String
getFirstPart str =
    String.left 4000 str



{- MiniLatex Table of contents -}


innerTableOfContents : MiniLatex.LatexState -> Html msg
innerTableOfContents latexState =
    Html.div [ HA.style "margin-top" "20px", HA.style "margin-left" "20px" ] (List.map innerTocItem (List.drop 1 latexState.tableOfContents))


innerTocItem : MiniLatex.TocEntry -> Html msg
innerTocItem tocEntry =
    let
        name =
            tocEntry.name |> String.replace " " "" |> String.toLower
    in
    Html.div [ HA.style "margin-bottom" "8px" ]
        [ Html.a [ HA.href <| "#_subsection_" ++ name, HA.style "font-size" "14px", HA.style "font-color" "blue" ]
            [ Html.text <| tocEntry.label ++ " " ++ tocEntry.name ]
        ]
