module Update.Document exposing
    ( downloadArchive
    , downloadFile
    , getFirstPart
    , makeNewDocument
    , processDocumentRequest
    , render
    , saveDocument
    , setCurrent
    , setCurrentSubdocument
    , text
    , updateMaybeUserWithDeque
    )

import BoundedDeque exposing (BoundedDeque)
import Cmd.Document
import Config
import Document exposing (Document)
import File.Download as Download
import Html exposing (Html)
import Interchange
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentListType(..)
        , EditMode(..)
        , Message
        , MessageType(..)
        , Model
        , Msg(..)
        , Visibility(..)
        )
import Prng.Uuid exposing (Uuid(..))
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Render
import Request exposing (RequestMsg(..))
import Toc exposing (TocItem)
import TocManager
import Tree exposing (Tree)
import Update.Render
import Update.Tool
import User exposing (User)


type alias HtmlRecord =
    { title : Html Msg, toc : Html Msg, document : Html Msg }


{-| When the text of a document is updated in the editor, update the following:

1.  update the document content and metadta
2.  the document AST and rendered text
3.  the current document, the document list, the table of contents and the deque of recent documents
4.  the text dirty flat

The document will not be saved to the backend until the current document save cycle completes
or the user intervenes to save manually.

-}
text : Model -> String -> ( Model, Cmd Msg )
text model str =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just doc ->
            let
                updatedDoc =
                    doc
                        |> Document.setContent str
                        |> Document.updateMetaData

                newDeque =
                    Document.pushFrontUnique updatedDoc model.deque

                tableOfContents =
                    Document.replaceInList updatedDoc model.tableOfContents
            in
            ( { model
                | -- document
                  currentDocument = Just updatedDoc
                , documentList = Document.replaceInList updatedDoc model.documentList
                , tableOfContents = tableOfContents
                , deque = newDeque
                , currentDocumentDirty = True
                , tocData = TocManager.setupWithFocus updatedDoc.id (List.head tableOfContents) (List.drop 1 tableOfContents)
                , tocCursor = Just updatedDoc.id

                -- rendering
                , renderingData = Render.update model.selectedId model.counter updatedDoc.content model.renderingData
                , counter = model.counter + 1
              }
            , Cmd.none
            )


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

        ( renderingData, renderCmd ) =
            Update.Render.prepare model (Just document)

        ( masterDocCmd, documentListDisplay_ ) =
            prepareIfMasterDoc model document
    in
    ( { model
        | currentDocument = Just document
        , documentListDisplay = documentListDisplay_
        , counter = model.counter + 2
        , deque = newDeque
        , currentUser = updateMaybeUserWithDeque newDeque model.currentUser
        , renderingData = renderingData
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
        |> Update.Tool.setupToEdit
    , Cmd.batch [ extraCmd, saveDocumentCommand, renderCmd, updateDequeCmd, masterDocCmd, Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfEditor ]
    )


setCurrentSubdocument : Model -> Document -> TocItem -> ( Model, Cmd Msg )
setCurrentSubdocument model document tocItem =
    let
        {- ... -}
        ( renderingData, renderCmd ) =
            Update.Render.prepare model (Just document)

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
        , renderingData = renderingData
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
        |> Update.Tool.setupToEdit
    , Cmd.batch [ renderCmd, loadChildrenCmd, saveDocumentCommand, Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfEditor ]
    )



--
--prepareAstAndRenderedText : Model -> Document -> ( Tree ParseWithId.MDBlockWithId, HtmlRecord, Cmd Msg )
--prepareAstAndRenderedText model document =
--    let
--        lastAst : Tree ParseWithId.MDBlockWithId
--        lastAst =
--            Markdown.ElmWithId.parse model.counter ExtendedMath document.content
--
--        nMath =
--            Markdown.ElmWithId.numberOfMathElements lastAst
--
--        renderedText : { title : Html msg, toc : Html msg, document : Html msg }
--        renderedText =
--            if nMath > 10 then
--                let
--                    firstAst =
--                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart document.content)
--                in
--                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" <| firstAst
--
--            else
--                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst
--
--        renderCmd =
--            if nMath > 10 then
--                Cmd.Document.renderAstFor lastAst
--
--            else
--                Cmd.none
--    in
--    ( lastAst, renderedText, renderCmd )
--
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

        ( renderingData, cmd ) =
            Update.Render.prepare model currentDoc
    in
    ( { model
        | documentList = documentList
        , currentDocument = currentDoc
        , tagString = getTagString currentDoc
        , counter = model.counter + 2
        , renderingData = renderingData
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


render : Model -> Document -> ( Model, Cmd Msg )
render model document =
    let
        ( renderingData, cmd1 ) =
            Update.Render.prepare model (Just document)

        cmd2 =
            if document.childInfo == [] then
                Cmd.none

            else
                Request.documentsInIdList Config.hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req
    in
    ( { model
        | renderingData = renderingData
        , counter = model.counter + 2
      }
    , Cmd.batch [ cmd1, cmd2 ]
    )


makeNewDocument : Model -> ( Model, Cmd Msg )
makeNewDocument model =
    case model.currentUser of
        Nothing ->
            ( model, Cmd.none )

        Just user ->
            let
                newDocumentText =
                    case model.docType of
                        Document.MiniLaTeX ->
                            "\\section{New Document}\n\nWrite something here (1) ..."

                        Document.Markdown _ ->
                            "# New Document\n\nWrite something here (1) ..."

                newDocument =
                    Document.create model.docType model.currentUuid user.username "New Document" newDocumentText

                ( newUuid, newSeed ) =
                    step Prng.Uuid.generator model.currentSeed

                newDeque =
                    Document.pushFrontUnique newDocument model.deque
            in
            ( { model
                | currentDocument = Just newDocument
                , documentList = newDocument :: model.documentList
                , visibilityOfTools = Visible
                , appMode = Editing StandardEditing
                , documentListDisplay = ( SearchResults, DequeViewOff )
                , tagString = ""
                , currentUuid = newUuid
                , currentSeed = newSeed
                , renderingData = Render.load model.selectedId model.counter (Render.documentOption newDocument) newDocument.content
                , deque = newDeque
              }
                |> Update.Tool.setupToEdit
            , Cmd.batch [ Request.insertDocument Config.hasuraToken newDocument |> Cmd.map Req, Cmd.Document.sendDequeOutside newDeque ]
            )


saveDocument : Model -> ( Model, Cmd Msg )
saveDocument model =
    case ( model.currentUser, model.currentDocument ) of
        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( Nothing, _ ) ->
            ( model, Cmd.none )

        ( Just user, Just document_ ) ->
            let
                document =
                    Document.updateMetaData document_
            in
            ( { model | message = ( UserMessage, "Saving document ..." ), currentDocument = Just document }
            , Request.updateDocument Config.hasuraToken user.username document |> Cmd.map Req
            )


downloadArchive : Model -> ( Model, Cmd Msg )
downloadArchive model =
    let
        currentUserName =
            Maybe.map .username model.currentUser

        userDocuments =
            List.filter (\doc -> Just doc.authorIdentifier == currentUserName) model.tableOfContents
    in
    ( model, Download.string "documents.json" "application/json" (Interchange.encodeDocumentList userDocuments) )


downloadFile : Model -> ( Model, Cmd Msg )
downloadFile model =
    let
        currentUserName =
            Maybe.map .username model.currentUser

        authorName =
            Maybe.map .authorIdentifier model.currentDocument
    in
    case ( currentUserName == authorName, model.currentDocument ) of
        ( True, Just doc ) ->
            ( model, Download.string (doc.title ++ ".txt") "application/txt" doc.content )

        ( _, _ ) ->
            ( model, Cmd.none )
