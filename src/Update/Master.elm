module Update.Master exposing
    ( loadSubdocument
    , processCandidateChildDocumentRequest
    , processChildDocumentRequest
    , setupOutline_
    , setupOutline
    )

import Cmd.Document
import Config
import Document exposing (Document)
import Markdown.ElmWithId
import Markdown.Option exposing (..)
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentListType(..)
        , Message
        , MessageType(..)
        , Model
        , Msg(..)
        )
import Request exposing (RequestMsg(..))
import TocManager
import Update.Document
import Update.Render


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
        content =
            document.content

        lastAst =
            Update.Render.parse document.docType model.counter content

        nMath =
            Markdown.ElmWithId.numberOfMathElements lastAst

        ( renderedText, cmd_ ) =
            if nMath > 1000 then
                let
                    firstAst =
                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (Update.Document.getFirstPart content)

                    renderedText_ =
                        Update.Render.render document.docType firstAst

                    cmd__ =
                        Cmd.Document.renderAstFor lastAst
                in
                ( renderedText_
                , cmd__
                )

            else
                ( Update.Render.render document.docType lastAst, Cmd.none )
    in
    ( { model
        | documentList = document :: List.filter (\doc -> doc.id /= document.id) model.documentList
        , currentDocument = Just document
        , tagString = getTagString (Just document)
        , counter = model.counter + 2
        , lastAst = lastAst
        , appMode = Reading
        , documentListDisplay = ( SearchResults, DequeViewOff )
        , renderedText = renderedText
        , docType = Document.getDocType (Just document)
        , message = ( UserMessage, "Success loading document" )
      }
    , cmd_
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
