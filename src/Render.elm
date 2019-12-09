module Render exposing (RenderingData(..), RenderingOption(..), documentOption, get, load, loadFast, render, update)

import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Html exposing (Html)
import Markdown.ElmWithId
import Markdown.Option as MDOption
import ParseWithId
import Render.Markdown
import Render.Types exposing (RenderedText)
import Tree exposing (Tree)


type RenderingData msg
    = MD (MData msg)


type RenderingOption
    = OMarkdown MDOption.Option


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
            OMarkdown MDOption.ExtendedMath


type alias MData msg =
    { option : MDOption.Option
    , renderedText : RenderedText msg
    , initialAst : Tree ParseWithId.MDBlockWithId
    , fullAst : Tree ParseWithId.MDBlockWithId
    }



-- markdownAst : RenderingData


load : Int -> RenderingOption -> String -> RenderingData msg
load counter option source =
    case option of
        OMarkdown opt ->
            loadMarkdown counter opt source


loadFast : Int -> RenderingOption -> String -> RenderingData msg
loadFast counter option source =
    case option of
        OMarkdown opt ->
            loadMarkdownFast counter opt source


render : RenderingData msg -> RenderingData msg
render rd =
    case rd of
        MD data ->
            MD { data | renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" data.fullAst }


update : Int -> String -> RenderingData msg -> RenderingData msg
update counter source rd =
    case rd of
        MD data ->
            let
                newAst =
                    Render.Markdown.diffUpdateAst data.option counter source data.fullAst
            in
            MD { data | fullAst = newAst, renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" newAst }


get : RenderingData msg -> RenderedText msg
get rd =
    case rd of
        MD data ->
            data.renderedText



{- HIDDEN -}


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


getFirstPart : String -> String
getFirstPart str =
    String.left 2000 str



--
--prepare : Int -> Maybe Document -> ( RenderingData msg, Cmd msg )
--prepare counter currentDoc =
--    case currentDoc of
--        Nothing ->
--            ( loadMarkdown MDOption.Extended "", Cmd.none )
--
--        Just doc ->
--            let
--                content =
--                    doc.content
--
--                lastAst =
--                    parse doc.docType counter content
--
--                nMath =
--                    Markdown.ElmWithId.numberOfMathElements lastAst
--
--                ( renderedText, cmd_ ) =
--                    if nMath > 10 then
--                        let
--                            firstAst =
--                                Markdown.ElmWithId.parse (counter + 1) ExtendedMath (getFirstPart content)
--
--                            renderedText_ =
--                                render doc.docType firstAst
--
--                            cmd__ =
--                                Cmd.Document.renderAstFor lastAst
--                        in
--                        ( renderedText_
--                        , cmd__
--                        )
--
--                    else
--                        ( render doc.docType lastAst, Cmd.none )
--            in
--            ( lastAst, renderedText, cmd_ )
