module Update.Master exposing
    ( addDocumentToMaster
    , addSubdocument
    , deleteSubdocument
    , firstSubdocument
    , loadSubdocument
    , newSubdocument
    , processCandidateChildDocumentRequest
    , processChildDocumentRequest
    , setupOutline
    , setupOutline_
    )

import Cmd.Document
import Config
import Document exposing (Document)
import List.Extra
import Markdown.ElmWithId
import Markdown.Option exposing (..)
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
import Prng.Uuid as Uuid exposing (Uuid(..))
import Random.Pcg.Extended exposing (Seed, step)
import Render
import Request exposing (RequestMsg(..))
import TocManager
import Update.Document
import Update.Render
import User exposing (User)


processChildDocumentRequest : Model -> List Document -> ( Model, Cmd Msg )
processChildDocumentRequest model documentList =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just masterDocument ->
            let
                newMaster_ =
                    TocManager.cleanChildInfo documentList masterDocument

                ( newMaster, cmd ) =
                    if List.length masterDocument.childInfo /= List.length newMaster_.childInfo then
                        ( newMaster_, Request.systemUpdateDocument Config.hasuraToken newMaster_ |> Cmd.map Req )

                    else
                        ( masterDocument, Cmd.none )

                --                newMaster = masterDocument
                --                cmd = Cmd.none
                sortedChildDocuments =
                    Document.sortChildren newMaster documentList

                newDocumentList =
                    newMaster :: sortedChildDocuments
            in
            ( { model
                | tableOfContents = newDocumentList
                , tocData = TocManager.setup (Just masterDocument) documentList
                , tocCursor = Just masterDocument.id
                , message = ( UserMessage, "Child documents: " ++ String.fromInt (List.length documentList) )
              }
            , cmd
            )


processCandidateChildDocumentRequest : Model -> List Document -> ( Model, Cmd Msg )
processCandidateChildDocumentRequest model documentList =
    ( { model
        | candidateChildDocumentList = documentList
        , message = ( UserMessage, "Candidate child documents: " ++ String.fromInt (List.length documentList) )
      }
    , Cmd.none
    )


{-| NOT USED ??
-}
loadSubdocument : Model -> Document -> ( Model, Cmd Msg )
loadSubdocument model document =
    let
        ( renderingData, cmd ) =
            Update.Render.prepare model (Just document)
    in
    ( { model
        | documentList = document :: List.filter (\doc -> doc.id /= document.id) model.documentList
        , currentDocument = Just document
        , tagString = getTagString (Just document)
        , counter = model.counter + 2
        , renderingData = renderingData
        , appMode = Reading
        , documentListDisplay = ( SearchResults, DequeViewOff )
        , docType = Document.getDocType (Just document)
        , message = ( UserMessage, "Success loading document" )
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


setupOutline : Model -> Model
setupOutline model =
    { model | documentOutline = setupOutline_ model }


setupOutline_ : Model -> String
setupOutline_ model =
    case model.currentDocument of
        Just currentDoc ->
            case TocManager.computeOutline currentDoc model.tableOfContents of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline

        Nothing ->
            model.documentOutline


addSubdocument : Model -> ( Model, Cmd Msg )
addSubdocument model =
    let
        maybeNewUuid =
            Uuid.fromString model.childDocIdString
    in
    case ( model.currentUser, model.currentDocument, maybeNewUuid ) of
        ( Just user, Just masterDocument, Just newUuid ) ->
            addSubdocument_ model user masterDocument newUuid

        ( _, _, _ ) ->
            ( model, Cmd.none )


deleteSubdocument : Model -> ( Model, Cmd Msg )
deleteSubdocument model =
    case ( List.head model.tableOfContents, model.currentDocument ) of
        ( Just masterDocument, Just documentToDelete ) ->
            deleteSubdocument_ model masterDocument documentToDelete

        ( _, _ ) ->
            ( model, Cmd.none )


deleteSubdocument_ : Model -> Document -> Document -> ( Model, Cmd Msg )
deleteSubdocument_ model masterDocument documentToDelete =
    let
        indexOfDocumentToDelete =
            TocManager.index documentToDelete masterDocument

        newMasterDocument =
            Document.deleteChild documentToDelete masterDocument

        newDocumentList =
            List.filter (\doc -> doc.id /= documentToDelete.id) model.documentList
                |> Document.replaceInList newMasterDocument

        tableOfContents =
            List.filter (\doc -> doc.id /= documentToDelete.id) model.tableOfContents
                |> Document.replaceInList newMasterDocument

        currentDocument =
            case indexOfDocumentToDelete of
                Nothing ->
                    newMasterDocument

                Just idx ->
                    if idx == 0 then
                        newMasterDocument

                    else
                        case List.Extra.getAt (idx - 1) (List.drop 1 tableOfContents) of
                            Nothing ->
                                newMasterDocument

                            Just doc ->
                                doc

        newDocumentOutline =
            case TocManager.computeOutline newMasterDocument tableOfContents of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline

        user =
            model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")
    in
    ( { model
        | currentDocument = Just currentDocument
        , tableOfContents = tableOfContents
        , documentList = newDocumentList
        , documentOutline = newDocumentOutline
        , tocData = TocManager.setupWithFocus masterDocument.id (Just masterDocument) (List.drop 1 tableOfContents)
        , tocCursor = Just currentDocument.id
      }
    , Request.updateDocument Config.hasuraToken user.username newMasterDocument |> Cmd.map Req
    )


addSubdocument_ : Model -> User -> Document -> Uuid.Uuid -> ( Model, Cmd Msg )
addSubdocument_ model user masterDocument newUuid =
    let
        newChildInfo =
            masterDocument.childInfo
                ++ [ ( newUuid, 0 ) ]

        newMasterDocument =
            { masterDocument | childInfo = newChildInfo }
    in
    ( { model | currentDocument = Just newMasterDocument, appMode = Reading }
    , Cmd.batch
        [ Request.updateDocument Config.hasuraToken user.username newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList Config.hasuraToken (newChildInfo |> List.map Tuple.first) GotChildDocuments |> Cmd.map Req
        ]
    )


addDocumentToMaster model document =
    case ( model.currentUser, model.currentDocument ) of
        ( Just user, Just master ) ->
            addDocumentToMaster_ model document master

        ( _, _ ) ->
            ( model, Cmd.none )


addDocumentToMaster_ model document master =
    let
        newChildInfo =
            master.childInfo
                ++ [ ( document.id, 0 ) ]

        newMasterDocument =
            { master | childInfo = newChildInfo }
    in
    let
        user =
            model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")
    in
    ( { model | currentDocument = Just newMasterDocument }
    , Cmd.batch
        [ Request.updateDocument Config.hasuraToken user.username newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList Config.hasuraToken (newChildInfo |> List.map Tuple.first) GotChildDocuments |> Cmd.map Req
        ]
    )


newSubdocument : Model -> ( Model, Cmd Msg )
newSubdocument model =
    case ( model.currentUser, List.head model.tableOfContents, model.currentDocument ) of
        ( Just user, Just masterDocument, Just currentDocument ) ->
            newSubdocument_ model user masterDocument currentDocument

        ( _, _, _ ) ->
            ( model, Cmd.none )


firstSubdocument : Model -> ( Model, Cmd Msg )
firstSubdocument model =
    case ( model.currentUser, model.currentDocument ) of
        ( Just user, Just document ) ->
            firstSubdocument_ model user document

        ( _, _ ) ->
            ( model, Cmd.none )


{-| Add a first subdocument to the given document
-}
firstSubdocument_ : Model -> User -> Document -> ( Model, Cmd Msg )
firstSubdocument_ model user document =
    let
        -- Prepare the new document
        newDocumentText =
            "# Title...\n\n___\n\n[Main](#id/" ++ Uuid.toString document.id ++ ")\n\n___\n\nText ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        renderingData =
            Render.load model.counter (Render.documentOption newDocument) newDocument.content

        ( newUuid, newSeed ) =
            step Uuid.generator model.currentSeed

        -- Prepare new master document, document lists, and document outlne
        masterDocument =
            { document | childInfo = [ ( newDocument.id, 0 ) ] }

        newChildDocumentList =
            [ newDocument ]

        newDocumentList =
            Document.replaceInList masterDocument (masterDocument :: newDocument :: model.documentList)

        newDocumentOutline =
            case TocManager.computeOutline masterDocument newChildDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline
    in
    ( { model
        | currentDocument = Just newDocument
        , tableOfContents = masterDocument :: newChildDocumentList
        , counter = model.counter + 1 -- Necessary?
        , documentList = newDocumentList
        , tocData = TocManager.setupWithFocus newDocument.id (Just masterDocument) newChildDocumentList
        , tocCursor = Just newDocument.id
        , documentListDisplay = ( DocumentChildren, DequeViewOff )
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , renderingData = renderingData
      }
    , Cmd.batch
        [ Request.insertDocument Config.hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument Config.hasuraToken user.username masterDocument |> Cmd.map Req
        ]
    )


newSubdocument_ : Model -> User -> Document -> Document -> ( Model, Cmd Msg )
newSubdocument_ model user masterDocument targetDocument =
    case masterDocument == targetDocument of
        True ->
            newSubdocumentAtHead model user masterDocument

        False ->
            newSubdocumentWithChildren model user masterDocument targetDocument


newSubdocumentAtHead : Model -> User -> Document -> ( Model, Cmd Msg )
newSubdocumentAtHead model user masterDocument =
    let
        -- Prepare the new document
        newDocumentText =
            "# Title...\n\n___\n\n[Main](#id/" ++ Uuid.toString masterDocument.id ++ ")\n\n___\n\nText ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        renderingData =
            Render.load model.counter (Render.documentOption newDocument) newDocument.content

        ( newUuid, newSeed ) =
            step Uuid.generator model.currentSeed

        -- Prepare new master document, document lists, and document outlne
        newMasterDocument =
            TocManager.insertInMasterAtHead newDocument masterDocument

        -- drop the master document for processing, then put it back after processing
        newChildDocumentList =
            newDocument :: List.drop 1 model.tableOfContents

        newDocumentList =
            Document.replaceInList newMasterDocument (newDocument :: model.documentList)

        newDocumentOutline =
            case TocManager.computeOutline newMasterDocument newChildDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline
    in
    ( { model
        | currentDocument = Just newDocument
        , tableOfContents = newMasterDocument :: newChildDocumentList
        , counter = model.counter + 1 -- Necessary?
        , documentList = newDocumentList
        , tocData = TocManager.setupWithFocus newDocument.id (Just newMasterDocument) newChildDocumentList
        , tocCursor = Just newDocument.id
        , documentListDisplay = ( DocumentChildren, DequeViewOff )
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , renderingData = renderingData
      }
    , Cmd.batch
        [ Request.insertDocument Config.hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument Config.hasuraToken user.username newMasterDocument |> Cmd.map Req
        ]
    )


newSubdocumentWithChildren : Model -> User -> Document -> Document -> ( Model, Cmd Msg )
newSubdocumentWithChildren model user masterDocument targetDocument =
    let
        -- Prepare the new document
        newDocumentText =
            "# Title...\n\n___\n\n[Main](#id/" ++ Uuid.toString masterDocument.id ++ ")\n\n___\n\nText ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        renderingData =
            Render.load model.counter (Render.documentOption newDocument) newDocument.content

        ( newUuid, newSeed ) =
            step Uuid.generator model.currentSeed

        -- Prepare new master document, document lists, and document outlne
        newMasterDocument =
            TocManager.insertInMaster newDocument targetDocument masterDocument

        -- drop the master document for processing, then put it back after processing
        newChildDocumentList =
            TocManager.insertInChildDocumentList newDocument targetDocument (List.drop 1 model.tableOfContents)

        newDocumentList =
            Document.replaceInList newMasterDocument (newDocument :: model.documentList)

        newDocumentOutline =
            case TocManager.computeOutline newMasterDocument newChildDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline
    in
    ( { model
        | currentDocument = Just newDocument
        , tableOfContents = newMasterDocument :: newChildDocumentList
        , counter = model.counter + 1
        , documentList = newDocumentList
        , tocData = TocManager.setupWithFocus newDocument.id (Just newMasterDocument) newChildDocumentList
        , tocCursor = Just newDocument.id
        , documentListDisplay = ( DocumentChildren, DequeViewOff )
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , renderingData = renderingData
      }
    , Cmd.batch
        [ Request.insertDocument Config.hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument Config.hasuraToken user.username newMasterDocument |> Cmd.map Req
        ]
    )
