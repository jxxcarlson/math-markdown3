module Render.Markdown exposing (diffUpdateAst, emptyAst, emptyRenderedText, getFirstPart, markdownOptionOfFlavor, parse, render)

-- import Cmd.Document

import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Markdown.ElmWithId
import Markdown.Option exposing (..)
import ParseWithId
import Render.Types exposing (RenderedText)
import Tree exposing (Tree)
import Tree.Diff as Diff


emptyAst : Tree ParseWithId.MDBlockWithId
emptyAst =
    Markdown.ElmWithId.parse -1 ExtendedMath ""


emptyRenderedText : RenderedText msg
emptyRenderedText =
    render (Markdown MDExtendedMath) emptyAst


parse : Option -> Int -> String -> Tree ParseWithId.MDBlockWithId
parse flavor counter str =
    Markdown.ElmWithId.parse counter flavor str


{-| compute a new AST from an old one and some text, preserving the ids of unchanged blocs.
The counter is used for the version number in the block ids.
-}
diffUpdateAst : Option -> Int -> String -> Tree ParseWithId.MDBlockWithId -> Tree ParseWithId.MDBlockWithId
diffUpdateAst option counter text lastAst =
    let
        newAst : Tree ParseWithId.MDBlockWithId
        newAst =
            parse option counter text
    in
    Diff.mergeWith ParseWithId.equal lastAst newAst


render : DocType -> Tree ParseWithId.MDBlockWithId -> RenderedText msg
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


getFirstPart : String -> String
getFirstPart str =
    String.left 2000 str
