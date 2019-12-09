module Update.Render exposing (prepare)

import Cmd.Document
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Markdown.Option exposing (..)
import Model exposing (Model, Msg(..))
import Render exposing (RenderingData(..), RenderingOption(..))



--
--
--emptyAst : Tree ParseWithId.MDBlockWithId
--emptyAst =
--    Markdown.ElmWithId.parse -1 ExtendedMath ""
--
--
--emptyRenderedText : RenderedText Msg
--emptyRenderedText =
--    render (Markdown MDExtendedMath) emptyAst
--
--
--parse : DocType -> Int -> String -> Tree ParseWithId.MDBlockWithId
--parse docType counter str =
--    case docType of
--        Markdown flavor ->
--            Markdown.ElmWithId.parse counter (markdownOptionOfFlavor flavor) str
--
--        _ ->
--            emptyAst
--
--
--{-| compute a new AST from an old one and some text, preserving the ids of unchanged blocs.
--The counter is used for the version number in the block ids.
---}
--diffUpdateAst : DocType -> Int -> String -> Tree ParseWithId.MDBlockWithId -> Tree ParseWithId.MDBlockWithId
--diffUpdateAst docType counter text lastAst =
--    let
--        newAst : Tree ParseWithId.MDBlockWithId
--        newAst =
--            parse docType counter text
--    in
--    Diff.mergeWith ParseWithId.equal lastAst newAst
--
--
--render : DocType -> Tree ParseWithId.MDBlockWithId -> RenderedText Msg
--render docType ast =
--    Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" ast
--
--
--markdownOptionOfFlavor : MarkdownFlavor -> Markdown.Option.Option
--markdownOptionOfFlavor flavor =
--    case flavor of
--        MDStandard ->
--            Standard
--
--        MDExtended ->
--            Extended
--
--        MDExtendedMath ->
--            ExtendedMath
--
--


prepare : Model -> Maybe Document -> ( RenderingData Msg, Cmd Msg )
prepare model currentDoc =
    case currentDoc of
        Nothing ->
            ( Render.load model.counter (OMarkdown Extended) "empty", Cmd.none )

        Just doc ->
            let
                option =
                    Render.documentOption doc
            in
            case String.length doc.content < 4000 of
                True ->
                    ( Render.load model.counter option doc.content, Cmd.none )

                False ->
                    let
                        rd =
                            Render.loadFast model.counter option doc.content
                    in
                    ( rd, Cmd.Document.renderAstFor rd )



--
--getFirstPart : String -> String
--getFirstPart str =
--    String.left 2000 str
