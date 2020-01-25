module Update.Render exposing (prepare)

import Cmd.Document
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Markdown.Option exposing (..)
import Model exposing (Model, Msg(..))
import Render exposing (RenderingData(..), RenderingOption(..))


prepare : Model -> Maybe Document -> ( RenderingData Msg, Cmd Msg )
prepare model currentDoc =
    case currentDoc of
        Nothing ->
            ( Render.load model.selectedId model.counter (OMarkdown Extended) "empty", Cmd.none )

        Just doc ->
            let
                option =
                    Render.documentOption doc
            in
            case String.length doc.content < 4000 of
                True ->
                    ( Render.load model.selectedId model.counter option doc.content, Cmd.none )

                False ->
                    let
                        rd =
                            Render.loadFast model.selectedId model.counter option doc.content
                    in
                    ( rd, Cmd.Document.renderAstFor model.selectedId rd )
