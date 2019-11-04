module Update.Render exposing (emptyAst, emptyRenderedText, parse, prepare, render)

import Cmd.Document
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Markdown.ElmWithId
import Markdown.Option exposing (..)
import Model exposing (Model, Msg(..), RenderedText)
import ParseWithId
import Tree exposing (Tree)


emptyAst : Tree ParseWithId.MDBlockWithId
emptyAst =
    Markdown.ElmWithId.parse -1 ExtendedMath ""


emptyRenderedText : RenderedText Msg
emptyRenderedText =
    render (Markdown MDExtendedMath) emptyAst


parse : DocType -> Int -> String -> Tree ParseWithId.MDBlockWithId
parse docType counter str =
    case docType of
        Markdown flavor ->
            Markdown.ElmWithId.parse counter (markdownOptionOfFlavor flavor) str

        _ ->
            emptyAst


render : DocType -> Tree ParseWithId.MDBlockWithId -> RenderedText Msg
render docType ast =
    Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" ast


markdownOptionOfFlavor : MarkdownFlavor -> Markdown.Option.Option
markdownOptionOfFlavor flavor =
    case flavor of
        MDStandard ->
            Standard

        MDExtended ->
            Extended

        MDExtendedMath ->
            ExtendedMath


prepare : Model -> Maybe Document -> ( Tree ParseWithId.MDBlockWithId, RenderedText Msg, Cmd Msg )
prepare model currentDoc =
    case currentDoc of
        Nothing ->
            ( emptyAst, emptyRenderedText, Cmd.none )

        Just doc ->
            let
                content =
                    doc.content

                lastAst =
                    parse doc.docType model.counter content

                nMath =
                    Markdown.ElmWithId.numberOfMathElements lastAst

                ( renderedText, cmd_ ) =
                    if nMath > 10 then
                        let
                            firstAst =
                                Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart content)

                            renderedText_ =
                                render doc.docType firstAst

                            cmd__ =
                                Cmd.Document.renderAstFor lastAst
                        in
                        ( renderedText_
                        , cmd__
                        )

                    else
                        ( render doc.docType lastAst, Cmd.none )
            in
            ( lastAst, renderedText, cmd_ )


getFirstPart : String -> String
getFirstPart str =
    String.left 2000 str
