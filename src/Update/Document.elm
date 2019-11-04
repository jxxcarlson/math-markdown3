module Update.Document exposing (getFirstPart, processDocumentRequest, setCurrent, setCurrentSubdocument, updateMaybeUserWithDeque)

import BoundedDeque exposing (BoundedDeque)
import Cmd.Document
import Config
import Document exposing (Document)
import Html exposing (Html)
import Markdown.ElmWithId
import Markdown.Option exposing (..)
import Model
    exposing
        ( DequeViewState(..)
        , DocumentListType(..)
        , Message
        , MessageType(..)
        , Model
        , Msg(..)
        )
import ParseWithId
import Request exposing (RequestMsg(..))
import Toc exposing (TocItem)
import Tree exposing (Tree)
import Update.Render
import User exposing (User)


type alias HtmlRecord =
    { title : Html Msg, toc : Html Msg, document : Html Msg }


setCurrent : Model -> Document -> Cmd Msg -> ( Model, Cmd Msg )
setCurrent model document extraCmd =
    -- XXX
    let
        -- When changing current document, be sure to save the current document if needed,
        -- and update the deque with the current version
        ( saveDocumentCommand, newDeque_ ) =
            case ( model.currentUser, model.currentDocument, model.currentDocumentDirty ) of
                ( Just user, Just docToSave, True ) ->
                    ( Request.updateDocument Config.hasuraToken user.username docToSave |> Cmd.map Req
                    , Document.pushFrontUnique docToSave model.deque
                    )

                ( _, _, _ ) ->
                    ( Cmd.none, model.deque )

        newDeque =
            Document.pushFrontUnique document newDeque_

        updateDequeCmd =
            case model.currentUser of
                Nothing ->
                    Cmd.none

                Just user ->
                    Request.updateUser Config.hasuraToken (updateUserWithDeque newDeque user) |> Cmd.map Req

        ( newAst, newRenderedText, renderCmd ) =
            prepareAstAndRenderedText model document

        ( masterDocCmd, documentListDisplay_ ) =
            prepareIfMasterDoc model document
    in
    ( { model
        | currentDocument = Just document
        , documentListDisplay = documentListDisplay_
        , counter = model.counter + 2
        , deque = newDeque
        , currentUser = updateMaybeUserWithDeque newDeque model.currentUser
        , lastAst = newAst
        , renderedText = newRenderedText
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
    , Cmd.batch [ extraCmd, saveDocumentCommand, renderCmd, updateDequeCmd, masterDocCmd, Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfEditor ]
    )


setCurrentSubdocument : Model -> Document -> TocItem -> ( Model, Cmd Msg )
setCurrentSubdocument model document tocItem =
    let
        {- Set the currently open node.
           At the moment this works only for the top level.
           That is, we change it only if it has level zero.
           At the moment this is necessary because otherwise if one
           clicks on an interior item, it would close the enclosing
           one.  One solution is to expose all positive levels, not
           must level 1.
        -}
        ( newAst, newRenderedText, renderCmd ) =
            prepareAstAndRenderedText model document

        ( masterDocCmd, documentListDisplay_ ) =
            prepareIfMasterDoc model document

        saveDocumentCommand =
            case ( model.currentUser, model.currentDocument, model.currentDocumentDirty ) of
                ( Just user, Just docToSave, True ) ->
                    Request.updateDocument Config.hasuraToken user.username docToSave |> Cmd.map Req

                ( _, _, _ ) ->
                    Cmd.none

        ( loadChildrenCmd, documentListDisplay__ ) =
            if document.childInfo == [] then
                ( Cmd.none, model.documentListDisplay )

            else
                ( Request.documentsInIdList Config.hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req, ( DocumentChildren, DequeViewOff ) )
    in
    ( { model
        | currentDocument = Just document
        , documentListDisplay = documentListDisplay_
        , counter = model.counter + 2
        , lastAst = newAst
        , renderedText = newRenderedText
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
    , Cmd.batch [ renderCmd, loadChildrenCmd, saveDocumentCommand, Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfEditor ]
    )


prepareAstAndRenderedText : Model -> Document -> ( Tree ParseWithId.MDBlockWithId, HtmlRecord, Cmd Msg )
prepareAstAndRenderedText model document =
    let
        lastAst : Tree ParseWithId.MDBlockWithId
        lastAst =
            Markdown.ElmWithId.parse model.counter ExtendedMath document.content

        nMath =
            Markdown.ElmWithId.numberOfMathElements lastAst

        renderedText : { title : Html msg, toc : Html msg, document : Html msg }
        renderedText =
            if nMath > 10 then
                let
                    firstAst =
                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart document.content)
                in
                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" <| firstAst

            else
                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst

        renderCmd =
            if nMath > 10 then
                Cmd.Document.renderAstFor lastAst

            else
                Cmd.none
    in
    ( lastAst, renderedText, renderCmd )



-- prepareIfMasterDoc : Model -> Document -> (Cmd Msg, DocumentListType)


prepareIfMasterDoc model document =
    if document.childInfo == [] then
        ( Cmd.none, model.documentListDisplay )

    else
        let
            cmd =
                Request.documentsInIdList Config.hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req
        in
        ( cmd, ( DocumentChildren, DequeViewOff ) )


updateUserWithDeque : BoundedDeque Document -> User -> User
updateUserWithDeque deque user =
    { user | recentDocs = BoundedDeque.toList deque |> List.map .id }


updateMaybeUserWithDeque : BoundedDeque Document -> Maybe User -> Maybe User
updateMaybeUserWithDeque deque maybeUser =
    Maybe.map (\user -> { user | recentDocs = BoundedDeque.toList deque |> List.map .id }) maybeUser


getFirstPart : String -> String
getFirstPart str =
    String.left 2000 str



-- COMPLETE DOCUMENT REQUEST


processDocumentRequest : Model -> Maybe Document -> List Document -> ( Model, Cmd Msg )
processDocumentRequest model maybeDocument documentList =
    let
        currentDoc =
            case maybeDocument of
                Nothing ->
                    List.head documentList

                Just doc_ ->
                    Just doc_

        ( newAst, newRenderedText, cmd ) =
            Update.Render.prepare model currentDoc
    in
    ( { model
        | documentList = documentList
        , currentDocument = currentDoc
        , tagString = getTagString currentDoc
        , counter = model.counter + 2
        , lastAst = newAst
        , renderedText = newRenderedText
        , docType = Document.getDocType currentDoc
        , message = ( UserMessage, "Success getting document list" )
      }
    , cmd
    )


getTagString : Maybe Document -> String
getTagString maybeDocument =
    case maybeDocument of
        Nothing ->
            ""

        Just document ->
            document.tags |> String.join ", "
