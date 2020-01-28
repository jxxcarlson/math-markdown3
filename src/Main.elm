module Main exposing (main)

import AppNavigation exposing (NavigationType(..))
import BoundedDeque exposing (BoundedDeque)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Cmd.Document
import Cmd.Extra exposing (withCmd, withCmds)
import Config
import Data
import Debounce
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Editor exposing (Editor, EditorConfig, EditorMsg)
import Editor.Update as E
import EditorTools
import Element exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Interchange
import Json.Encode as E
import Keyboard exposing (Key(..))
import KeyboardManager
import Markdown.ElmWithId
import Markdown.Option as MDOption exposing (Option(..))
import Markdown.Parse as Parse
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentDeleteState(..)
        , DocumentListDisplay
        , DocumentListType(..)
        , EditMode(..)
        , FocusedElement(..)
        , Message
        , MessageType(..)
        , Model
        , Msg(..)
        , SearchMode(..)
        , SearchType(..)
        , SortMode(..)
        , UserState(..)
        , Visibility(..)
        )
import Outside
import Preprocessor
import Prng.Uuid as Uuid exposing (Uuid)
import Random
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData(..))
import Render exposing (MDData, MLData, RenderingData(..), RenderingOption(..))
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..), orderByMostRecentFirst, orderByTitleAsc)
import Search
import Sync
import Task exposing (Task)
import Time exposing (Posix)
import TocManager
import TocZ exposing (TocMsg(..))
import Tree.Diff as Diff
import Update.Document
import Update.Master
import Update.Render
import Update.Tool
import Update.UI
import Url exposing (Url)
import User exposing (AuthorizedUser, User)
import Utility
import View.Editor
import View.Reader
import View.User



{-

   OUTLINE

      MODEL
      TYPES FOR MODEL
      INIT
      MSG
      SUBSCRIPTIONS
      UPDATE FUNCTION
      KEYBOARD HELPERS
      TIME HELPERS

      OUTSIDE HELPERS

      EDITOR HELPERS
      DOCUMENT HELPERS
      PARSE AND RENDER
      UI HELPERS: scale, etc.

      USER PAGE
      SIGN-IN-UP-OUT
      SUBDOCUMENT EDITOR
      TEXT EDITOR
      READER

      DOCUMENT TOOL PANEL: BUTTONS AND INPUTS
      DOCUMENT LIST
      TABLE OF CONTENTS
      HEADERS


      VIEW UTILITIES: Utility.View.showIf, etc.


-}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 100
    , transform = DebounceMsg
    }



-- TYPES FOR MODEL


type UrlRequest
    = Internal Url.Url
    | External String


type alias Flags =
    { width : Int
    , height : Int
    , seed : Int
    , randInts : List Int
    , location : String
    }



-- CONFIGURATION


config =
    { debounceInterval = 500
    , timeoutInMs = 5 * 1000
    , panelHeight = 550
    , panelWidth = 450
    , maxFlashCount = 30
    , dequeLength = 10
    }


hasuraToken : String
hasuraToken =
    "GOc97wA7CCMm31H4UJHa-4pqdVoLf3l6gAwzczdHC"


viewInfoEditing =
    { toolStripWidth = 0.05
    , docListWidth = 0.15
    , editorWidth = 0.3
    , renderedDisplayWidth = 0.3
    , tocWidth = 0.2
    , vInset = vInset
    , hExtra = 0
    }


viewInfoEditingSubdocuemnt =
    { toolStripWidth = 0.05
    , docListWidth = 0.25
    , editorWidth = 0.3
    , renderedDisplayWidth = 0.3
    , tocWidth = 0.1
    , vInset = vInset
    , hExtra = 0
    }


viewInfoReading =
    { toolStripWidth = 0.05
    , docListWidth = 0.25
    , editorWidth = 0
    , renderedDisplayWidth = 0.45
    , tocWidth = 0.25
    , vInset = vInset
    , hExtra = 0
    }


viewInfoUserPage =
    { lhsFraction = 0.45
    , rhsFraction = 0.55
    , vInset = vInset
    }


vInset =
    75



-- INIT --


editorWindowHeight : Float -> Float
editorWindowHeight windowHeight =
    windowHeight - 90


px : Float -> String
px k =
    String.fromFloat k ++ "px"


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator (initialSeed flags.seed flags.randInts)

        initialAst =
            Parse.toMDBlockTree -1 ExtendedMath Data.loadingPage.content

        newEditorConfig =
            Model.defaultEditorConfig
                |> Model.setEditorDimensions flags.height

        model_ : Model
        model_ =
            { seed = 0
            , key = key
            , url = url
            , debounce = Debounce.init

            -- UI
            , docType = Markdown MDExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , visibilityOfTools = Invisible
            , appMode = UserMode SignInState
            , documentListDisplay = ( SearchResults, DequeViewOff )
            , message = ( UserMessage, "Starting ..." )
            , pressedKeys = []
            , focusedElement = NoFocus
            , flashCounterForTotalWordCount = 0
            , flashCounterForShareUrl = 0

            -- SYSTEM
            , currentSeed = newSeed -- initialSeed flags.seed flags.randInts
            , currentUuid = newUuid -- Nothing
            , zone = Time.utc
            , time = Time.millisToPosix 0

            -- USER
            , currentUser = Nothing

            --, maybeUser = Nothing
            , token = Nothing
            , username = ""
            , email = ""
            , password = ""
            , passwordConfirmation = ""
            , newPassword1 = ""
            , newPassword2 = ""

            -- EDITOR
            , selectedText = ""
            , selectedId = ( 0, 0 )
            , editorTargetLineNumber = Nothing
            , clipboard = ""

            -- documents
            , counter = 1
            , documentDeleteState = SafetyOn
            , documentList = [ Data.loadingPage ]
            , tableOfContents = []
            , deque = BoundedDeque.empty config.dequeLength
            , totalWordCount = 0
            , tocData = Nothing
            , tocCursor = Nothing
            , toggleToc = False
            , candidateChildDocumentList = []
            , childDocIdString = ""
            , currentDocument = Just Data.loadingPage
            , currentDocumentDirty = False
            , secondsWhileDirty = 0
            , renderingData = Render.load ( 0, 0 ) 0 (OMarkdown MDOption.Standard) "Empty document"
            , tagString = ""
            , searchTerms = ""
            , sortTerm = orderByMostRecentFirst
            , searchMode = PublicSearch
            , sortMode = MostRecentFirst
            , documentOutline = ""
            , usernameToAddToPermmission = ""
            , permissionToAdd = NoPermission
            , editor = Editor.init Model.defaultEditorConfig "Some text" -- |> setEditorHeight model.windowHeight
            , editorConfig = Model.defaultEditorConfig |> Model.setEditorDimensions flags.height
            }
    in
    ( model_
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Cmd.Document.resetViewportOfRenderedText
        , Cmd.Document.resetViewportOfEditor
        , Outside.sendInfo (Outside.AskToReconnectUser E.null)
        , Outside.sendInfo (Outside.AskForDequeData E.null)
        , Cmd.Document.processUrl flags.location
        ]
    )


bareModel : Model -> Model
bareModel model =
    let
        initialAst =
            Parse.toMDBlockTree -1 ExtendedMath Data.loadingPage.content
    in
    { model
        | -- UI
          docType = Markdown MDExtendedMath
        , visibilityOfTools = Invisible
        , appMode = UserMode SignInState
        , documentListDisplay = ( SearchResults, DequeViewOff )
        , message = ( UserMessage, "Starting ..." )
        , pressedKeys = []
        , focusedElement = NoFocus
        , flashCounterForTotalWordCount = 0

        -- SYSTEM
        -- USER
        , currentUser = Nothing

        --, maybeUser = Nothing
        , token = Nothing
        , username = ""
        , email = ""
        , password = ""
        , passwordConfirmation = ""
        , newPassword1 = ""
        , newPassword2 = ""

        -- EDITOR
        , selectedText = ""
        , editorTargetLineNumber = Nothing

        -- documents
        , counter = 0
        , documentDeleteState = SafetyOn
        , documentList = [ Data.loadingPage ]
        , tableOfContents = []
        , deque = BoundedDeque.empty config.dequeLength
        , totalWordCount = 0
        , tocData = Nothing
        , tocCursor = Nothing
        , toggleToc = False
        , candidateChildDocumentList = []
        , childDocIdString = ""
        , currentDocument = Just Data.loadingPage
        , currentDocumentDirty = False
        , secondsWhileDirty = 0
        , renderingData = Render.load ( 0, 0 ) 0 (OMarkdown MDOption.Standard) "Empty document"
        , tagString = ""
        , searchTerms = ""
        , sortTerm = orderByMostRecentFirst
        , searchMode = PublicSearch
        , sortMode = MostRecentFirst
        , documentOutline = ""
        , usernameToAddToPermmission = ""
        , permissionToAdd = NoPermission
    }



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Config.tick Tick
        , Browser.Events.onResize WindowSize
        , Sub.map KeyMsg Keyboard.subscriptions
        , Outside.getInfo Outside LogErr
        ]



-- UPDATE FUNCTION


updateTextTask : String -> Cmd Msg
updateTextTask str =
    Task.perform UpdateDocumentText (Task.succeed str)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Test ->
            ( model, Dom.getViewportOf "__inner_editor__" |> Task.attempt GotViewport )

        GotViewport result ->
            case result of
                Ok vp ->
                    --                    let
                    --                        _ =
                    --                            Debug.log "VP" vp
                    --                    in
                    ( model, Cmd.none )

                Err _ ->
                    --                    let
                    --                        _ =
                    --                            Debug.log "GotViewport" "error"
                    --                    in
                    ( model, Cmd.none )

        DebounceMsg debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast updateTextTask)
                        debounceMsg
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        -- EDITOR II
        Rerender _ ->
            let
                newSource =
                    Editor.getSource model.editor
            in
            model
                |> Update.Document.text newSource
                |> withCmd Cmd.none

        EditorMsg editorMsg ->
            let
                ( newEditor, editorCmd ) =
                    Editor.update editorMsg model.editor
            in
            case editorMsg of
                E.Insert str ->
                    updateEditor model newEditor editorCmd

                E.WriteToSystemClipBoard ->
                    ( { model | editor = newEditor }
                    , Cmd.batch
                        [ Outside.sendInfo (Outside.WriteToClipBoard (Editor.getSelectedText newEditor |> Maybe.withDefault "Nothing!!"))
                        , editorCmd |> Cmd.map EditorMsg
                        ]
                    )

                E.Unload str ->
                    syncWithEditor model newEditor editorCmd

                E.RemoveCharAfter ->
                    syncWithEditor model newEditor editorCmd

                E.RemoveCharBefore ->
                    syncWithEditor model newEditor editorCmd

                E.Cut ->
                    syncWithEditor model newEditor editorCmd

                E.Paste ->
                    syncWithEditor model newEditor editorCmd

                E.Undo ->
                    syncWithEditor model newEditor editorCmd

                E.Redo ->
                    syncWithEditor model newEditor editorCmd

                E.WrapSelection ->
                    syncWithEditor model newEditor editorCmd

                E.Clear ->
                    syncWithEditor model newEditor editorCmd

                E.WrapAll ->
                    syncWithEditor model newEditor editorCmd

                E.SendLine ->
                    syncAndHighlightRenderedText (Editor.lineAtCursor newEditor) (editorCmd |> Cmd.map EditorMsg) { model | editor = newEditor }

                E.CopyPasteClipboard ->
                    ( { model | editor = newEditor }
                    , Cmd.batch
                        [ Outside.sendInfo (Outside.AskForClipBoard E.null)
                        , editorCmd |> Cmd.map EditorMsg
                        ]
                    )

                E.ReplaceCurrentSelection ->
                    syncWithEditor model newEditor editorCmd

                _ ->
                    updateEditor model newEditor editorCmd

        PasteClipboard ->
            pasteToClipboard model

        AskForClipBoard ->
            ( model, Outside.sendInfo (Outside.AskForClipBoard E.null) )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        -- PORTS --
        Outside infoForElm ->
            case infoForElm of
                Outside.UserDataFromOutside outsideUser ->
                    let
                        user =
                            User.fromOutside outsideUser
                    in
                    ( { model
                        | currentUser = Just user
                        , appMode = Reading
                        , documentListDisplay = ( SearchResults, DequeViewOn )
                        , focusedElement = NoFocus
                        , visibilityOfTools = Invisible
                        , searchMode = UserSearch
                        , token = Just outsideUser.token
                      }
                    , Cmd.Document.getUserDocumentsAtSignIn user
                    )

                Outside.GotSelection selection ->
                    let
                        maybeLineNumber =
                            case model.currentDocument of
                                Nothing ->
                                    Nothing

                                Just doc ->
                                    EditorTools.lineNumber (String.left 16 selection) doc.content

                        ( message, cmd ) =
                            case maybeLineNumber of
                                Just k ->
                                    ( "Line number: " ++ String.fromInt k, Outside.sendInfo (Outside.ScrollToLine (E.int k)) )

                                Nothing ->
                                    ( "Could not find line", Cmd.none )
                    in
                    ( { model | editorTargetLineNumber = maybeLineNumber, selectedText = selection, message = ( UserMessage, message ) }
                    , cmd
                    )

                Outside.GotSelectionForSync selection_ ->
                    let
                        selection =
                            String.trim selection_

                        ( newEditor, editorCmd ) =
                            Editor.scrollToString selection model.editor
                    in
                    syncAndHighlightRenderedText
                        (Editor.lineAtCursor newEditor)
                        (editorCmd |> Cmd.map EditorMsg)
                        { model | selectedText = selection, editor = newEditor }

                Outside.GotClipboard clipboard ->
                    pasteToEditorClipboard model clipboard

                Outside.UuidList uuidList ->
                    ( model, Request.documentsInIdList hasuraToken uuidList GotDequeDocuments |> Cmd.map Req )

        LogErr err ->
            ( { model | message = ( ErrorMessage, err ) }, Cmd.none )

        Model.Clear ->
            ( { model
                | counter = model.counter + 1
              }
            , Cmd.none
            )

        SetDocumentListType documentListType ->
            let
                dv =
                    Tuple.second model.documentListDisplay
            in
            case documentListType of
                SearchResults ->
                    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ) }, Cmd.none )

                DocumentChildren ->
                    ( { model | documentListDisplay = ( DocumentChildren, DequeViewOff ) }, Cmd.none )

        SetDequeview dv ->
            let
                slt =
                    Tuple.first model.documentListDisplay
            in
            ( { model | documentListDisplay = ( slt, dv ) }, Cmd.none )

        ToggleDequeview ->
            let
                newDocumentListDisplay =
                    case model.documentListDisplay of
                        ( slt, DequeViewOn ) ->
                            ( slt, DequeViewOff )

                        ( slt, DequeViewOff ) ->
                            ( slt, DequeViewOn )
            in
            ( { model | documentListDisplay = newDocumentListDisplay }, Cmd.none )

        SetSortMode sortMode ->
            let
                sortTerm =
                    case sortMode of
                        Alphabetical ->
                            orderByTitleAsc

                        MostRecentFirst ->
                            orderByMostRecentFirst
            in
            Search.do { model | sortMode = sortMode, sortTerm = sortTerm }

        SetDocType docType ->
            case model.currentDocument of
                Nothing ->
                    ( model, Cmd.none )

                Just doc ->
                    reloadDocument model { doc | docType = docType }

        SetToolPanelState visibility ->
            ( { model | visibilityOfTools = visibility }, Cmd.none )

        SetAppMode appMode ->
            case appMode of
                Reading ->
                    Update.UI.setModeToReading model

                Editing editMode ->
                    Update.UI.setModeToEditing model editMode

                UserMode s ->
                    Update.UI.setUserMode model s

        -- SYSTEM --
        NewUuid ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.generator model.currentSeed
            in
            -- 2.: Store the new seed
            ( { model
                | currentUuid = newUuid
                , currentSeed = newSeed
              }
            , Cmd.none
            )

        Tick newTime ->
            handleTime model newTime

        WindowSize width height ->
            ( { model
                | windowWidth = width
                , windowHeight = height
                , editorConfig = Model.defaultEditorConfig |> Model.setEditorDimensions height
              }
            , Cmd.none
            )

        KeyMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange
                        (Keyboard.oneOf [ Keyboard.characterKeyOriginal, Keyboard.modifierKey, Keyboard.whitespaceKey ])
                        keyMsg
                        model.pressedKeys
            in
            KeyboardManager.keyboardGateway model ( pressedKeys, maybeKeyChange )

        SetFocusOnSearchBox result ->
            ( model, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        -- USER --
        GotUserName str ->
            ( { model | username = str }, Cmd.none )

        GotPassword str ->
            ( { model | password = str }, Cmd.none )

        GotPasswordConfirmation str ->
            ( { model | passwordConfirmation = str }, Cmd.none )

        GotNewPassword1 _ ->
            ( model, Cmd.none )

        GotNewPassword2 _ ->
            ( model, Cmd.none )

        ChangePassword ->
            ( model, Cmd.none )

        GotEmail str ->
            ( { model | email = str }, Cmd.none )

        SignIn ->
            signIn model

        SignUp ->
            ( { model | appMode = UserMode SignUpState, message = ( AuthMessage, "Signing up ..." ) }
            , Request.signUpUser model.username model.email model.password model.passwordConfirmation |> Cmd.map Req
            )

        SignOut ->
            ( bareModel model
            , Outside.sendInfo (Outside.DestroyUserData E.null)
            )

        -- EDITOR --
        ProcessLine str ->
            let
                id =
                    case EditorTools.findStringInAST str model.renderingData of
                        Nothing ->
                            "??"

                        Just id_ ->
                            id_ |> Parse.stringOfId
            in
            ( { model | message = ( UserMessage, "str = " ++ String.left 20 str ++ " -- Clicked on id: " ++ id ) }
            , Cmd.Document.setViewportForElement id
            )

        SyncEditorToLine k ->
            ( model, Cmd.none )

        SetViewPortForElement result ->
            case result of
                Ok ( element, viewport ) ->
                    ( model, Cmd.Document.setViewPortForSelectedLine element viewport )

                Err _ ->
                    ( { model | message = ( ErrorMessage, Tuple.second model.message ) }, Cmd.none )

        GetTextSelection ->
            ( model, Outside.sendInfo (Outside.GetTextSelectionFromOutside E.null) )

        GetSelectionForSync ->
            ( model, Outside.sendInfo (Outside.GetSelectionForSyncOutside E.null) )

        DownloadArchive ->
            Update.Document.downloadArchive model

        DownloadFile ->
            Update.Document.downloadFile model

        ArchiveRequested ->
            ( model, Select.file [ "application/json" ] ArchiveSelected )

        ArchiveSelected file ->
            ( model, Task.perform ArchiveLoaded (File.toString file) )

        ArchiveLoaded archiveString ->
            let
                importedDocuments =
                    Interchange.decodeDocumentList archiveString |> Maybe.withDefault []

                author =
                    List.map .authorIdentifier importedDocuments |> List.head |> Maybe.withDefault "NoAuthor"

                message =
                    ( UserMessage, "Imported docs:  " ++ String.fromInt (List.length importedDocuments) ++ " (by " ++ author ++ ")" )
            in
            case List.length importedDocuments > 0 of
                False ->
                    ( { model | message = message }, Cmd.none )

                True ->
                    let
                        ( renderingData, cmd ) =
                            Update.Render.prepare model (List.head importedDocuments)
                    in
                    ( { model
                        | documentList = importedDocuments
                        , currentDocument = List.head importedDocuments
                        , renderingData = renderingData
                        , counter = model.counter + 2
                        , message = message
                      }
                    , cmd
                    )

        SaveImportedArchive ->
            let
                insertDoc =
                    \doc -> Request.insertDocument Config.hasuraToken doc |> Cmd.map Req

                cmdList =
                    List.map insertDoc model.documentList
            in
            ( model, Cmd.batch cmdList )

        CreateDocument ->
            Update.Document.makeNewDocument model

        NewSubdocument ->
            Update.Master.newSubdocument model

        FirstSubdocument ->
            Update.Master.firstSubdocument model

        AddSubdocument ->
            Update.Master.addSubdocument model

        DeleteSubdocument ->
            Update.Master.deleteSubdocument model

        SaveDocument ->
            Update.Document.saveDocument model

        ArmForDelete ->
            ( { model | documentDeleteState = Armed, message = ( UserMessage, "Armed for delete.  Caution!" ) }, Cmd.none )

        CancelDeleteDocument ->
            ( { model | documentDeleteState = SafetyOn, message = ( UserMessage, "Delete cancelled" ) }, Cmd.none )

        DeleteDocument ->
            deleteDocument model

        GetUserDocuments ->
            Search.forUsersDocuments model

        GotSecondPart rd ->
            ( { model | renderingData = rd }, Cmd.none )

        AllDocuments ->
            Search.getAllDocuments model

        GetPublicDocuments ->
            Search.forPublicDocuments model

        GetHelpDocs ->
            Search.getHelpDocs model

        AddThisDocumentToMaster document ->
            Update.Master.addDocumentToMaster model document

        GotOutline str ->
            ( { model | documentOutline = str }, Cmd.none )

        UpdateDocumentText str ->
            model
                |> Update.Document.text (Preprocessor.apply str)
                |> withCmd Cmd.none

        SetCurrentDocument document ->
            Update.Document.setCurrent model document (Cmd.Document.sendDequeOutside model.deque)

        SetCurrentSubDocument document tocItem ->
            Update.Document.setCurrentSubdocument model document tocItem

        SetUpOutline ->
            ( Update.Master.setupOutline model, Cmd.none )

        AddUserNameForPermissions str ->
            ( { model | usernameToAddToPermmission = str }, Cmd.none )

        CyclePermission ->
            let
                nextPermission =
                    case model.permissionToAdd of
                        NoPermission ->
                            ReadPermission

                        ReadPermission ->
                            WritePermission

                        WritePermission ->
                            NoPermission
            in
            ( { model | permissionToAdd = nextPermission }, Cmd.none )

        AddUserPermission ->
            let
                _ =
                    "ADD clicked"
            in
            addUserPermission model

        UpdateChildren ->
            case List.head model.tableOfContents of
                Nothing ->
                    ( model, Cmd.none )

                Just masterDocument ->
                    case TocManager.updateMasterAndDocumentListFromOutline model.documentOutline model.tableOfContents of
                        Err error ->
                            ( { model | message = ( ErrorMessage, Document.stringOfError error ) }, Cmd.none )

                        Ok ( newMasterDocument, newDocumentList ) ->
                            let
                                user =
                                    model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")
                            in
                            ( { model
                                | currentDocument = Just newMasterDocument
                                , tableOfContents = newDocumentList
                                , tocData = TocManager.setupWithFocus masterDocument.id (Just newMasterDocument) (List.drop 1 newDocumentList)
                                , tocCursor = Just masterDocument.id
                              }
                            , Request.updateDocument hasuraToken user.username newMasterDocument |> Cmd.map Req
                            )

        SetDocumentPublic bit ->
            setDocumentPublic model bit

        GotTagString str ->
            processTagString model str

        GotChildDocIdString str ->
            ( { model | childDocIdString = str }, Cmd.none )

        DoTotalWordCount ->
            ( { model | totalWordCount = Document.totalWordCount model.tableOfContents, flashCounterForTotalWordCount = config.maxFlashCount }, Cmd.none )

        DoShareUrl ->
            ( { model | flashCounterForShareUrl = config.maxFlashCount }, Cmd.none )

        GotSearchTerms str ->
            ( { model | searchTerms = str, focusedElement = FocusOnSearchBox }, Cmd.none )

        DoSearch ->
            Search.do model

        ToggleSearchMode ->
            Search.cycleSearchMode model

        ClearSearchTerms ->
            Search.clearSearchTerms model

        TOC tocMsg ->
            case tocMsg of
                Focus id ->
                    focusOnId model id

                Toggle ->
                    ( { model | toggleToc = not model.toggleToc }, Cmd.batch [ Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfRenderedText ] )

        -- Cmd.none |> Cmd.map TOC )
        -- NAVIGATION --
        -- XYXY1
        ScrollAttempted _ ->
            ( model
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                id =
                    String.replace "%20" " " (Maybe.withDefault "foo" url.fragment)
            in
            handleLink model id

        -- REQ --
        Req requestMsg ->
            case requestMsg of
                UpdateDocumentResponse (GraphQLResponse remoteData) ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Update doc: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Update doc: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Update doc: failed request" ) }, Cmd.none )

                        Success _ ->
                            ( { model
                                | message = ( UserMessage, "Document update successful" )
                                , currentDocumentDirty = False
                                , secondsWhileDirty = 0
                              }
                            , Cmd.none
                            )

                DeleteDocumentResponse (GraphQLResponse remoteData) ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Delete doc: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Delete doc: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Delete doc: failed request" ) }, Cmd.none )

                        Success _ ->
                            handleDeletedDocument model

                GotUserDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get author docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            Update.Document.processDocumentRequest model Nothing documentList

                GotChildDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get child docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            Update.Master.processChildDocumentRequest model documentList

                GotDequeDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get child docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            let
                                _ =
                                    "SETTING UO DEQUE"

                                newDeque =
                                    BoundedDeque.fromList config.dequeLength documentList

                                currentUser =
                                    if BoundedDeque.isEmpty newDeque then
                                        model.currentUser

                                    else
                                        Update.Document.updateMaybeUserWithDeque newDeque model.currentUser

                                cmd =
                                    case BoundedDeque.isEmpty newDeque of
                                        False ->
                                            Cmd.none

                                        True ->
                                            let
                                                docIds =
                                                    case model.currentUser of
                                                        Nothing ->
                                                            []

                                                        Just user ->
                                                            user.recentDocs
                                            in
                                            Request.documentsInIdList hasuraToken docIds GotDocumentsForDeque |> Cmd.map Req
                            in
                            ( { model | deque = newDeque, currentUser = currentUser }, cmd )

                GotCandidateChildDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get child docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            Update.Master.processCandidateChildDocumentRequest model documentList

                GotPublicDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get author docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            let
                                currentDoc =
                                    case Document.getById "e6880153-cc25-48b6-9d71-aae6653aad23" documentList of
                                        Nothing ->
                                            List.head documentList

                                        Just document ->
                                            Just document

                                ( newModel, cmd ) =
                                    Update.Document.processDocumentRequest model currentDoc documentList
                            in
                            ( { newModel | currentDocument = currentDoc }, cmd )

                GotPublicDocuments2 remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get author docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get author docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            Update.Document.processDocumentRequest model Nothing documentList

                InsertDocumentResponse _ ->
                    ( { model | message = ( UserMessage, "New document saved" ) }, Cmd.none )

                GotUserSignUp result ->
                    case result of
                        Ok (AuthToken token) ->
                            let
                                ( newUuid, newSeed ) =
                                    step Uuid.generator model.currentSeed

                                newUser =
                                    User.barebonesUser model.currentUuid model.username model.email
                            in
                            ( { model
                                | token = Just token
                                , currentUser = Just newUser
                                , currentUuid = newUuid
                                , currentSeed = newSeed
                                , username = ""
                                , email = ""
                                , appMode = UserMode SignedInState
                                , message = ( UserMessage, "Signup successful" )
                              }
                            , Request.insertUser hasuraToken newUser |> Cmd.map Req
                            )

                        Ok (AuthError msg_) ->
                            ( { model | token = Nothing, message = ( AuthMessage, msg_ ) }, Cmd.none )

                        Err error ->
                            ( { model | token = Nothing, message = ( UserMessage, Request.stringFromHttpError error ) }, Cmd.none )

                GotUserSignIn result ->
                    case result of
                        Ok (AuthToken token) ->
                            let
                                username =
                                    model.username
                            in
                            ( { model
                                | token = Just token
                                , username = ""
                                , message = ( UserMessage, "Sign-in successful" )
                              }
                            , Request.getUserByUsername hasuraToken username |> Cmd.map Req
                            )

                        Ok (AuthError msg_) ->
                            ( { model | token = Nothing, message = ( AuthMessage, msg_ ) }, Cmd.none )

                        Err error ->
                            ( { model | token = Nothing, message = ( UserMessage, Request.stringFromHttpError error ) }, Cmd.none )

                InsertUserResponse (GraphQLResponse remoteData) ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Update doc: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Update doc: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Update doc: failed request" ) }, Cmd.none )

                        Success user ->
                            ( { model
                                | message = ( UserMessage, "User signup successful (2)" )
                              }
                            , Cmd.none
                            )

                LoadDocument remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "LoadDocument: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "LoadDocument: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "LoadDocument: failed request" ) }, Cmd.none )

                        Success documentList ->
                            case List.head documentList of
                                Nothing ->
                                    ( { model | message = ( UserMessage, "Couldn't load linked document" ) }, Cmd.none )

                                Just document ->
                                    loadDocument model document

                GotDocumentsForDeque remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get deque: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get deque: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get deque: failed request" ) }, Cmd.none )

                        Success documentList ->
                            let
                                newDeque =
                                    BoundedDeque.fromList config.dequeLength documentList
                            in
                            ( { model
                                | message = ( UserMessage, "Got deque: successful" )
                                , currentUser = Update.Document.updateMaybeUserWithDeque newDeque model.currentUser
                                , deque = newDeque
                              }
                            , Cmd.none
                            )

                GotUserAtSignin remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Update doc: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Update doc: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Update doc: failed request" ) }, Cmd.none )

                        Success userList ->
                            case List.head userList of
                                Nothing ->
                                    ( { model
                                        | message = ( ErrorMessage, "Could not match authorized user in Hasura" )
                                        , currentUser = Nothing
                                      }
                                    , Cmd.none
                                    )

                                Just user ->
                                    let
                                        tokenCmd =
                                            case model.token of
                                                Nothing ->
                                                    Cmd.none

                                                Just token ->
                                                    Outside.sendInfo (Outside.UserData <| User.outsideUserEncoder (User.outsideUserWithToken token user))

                                        dequeCommand =
                                            Request.documentsInIdList hasuraToken user.recentDocs GotDocumentsForDeque |> Cmd.map Req
                                    in
                                    ( { model
                                        | message = ( UserMessage, "User signup successful (2)" )
                                        , currentUser = Just user
                                        , appMode = Reading
                                        , documentListDisplay = ( SearchResults, DequeViewOn )
                                        , focusedElement = NoFocus
                                        , visibilityOfTools = Invisible
                                      }
                                    , Cmd.batch
                                        [ Cmd.Document.getUserDocumentsAtSignIn user
                                        , Cmd.batch [ tokenCmd, dequeCommand ]
                                        ]
                                    )


pasteToClipboard : Model -> ( Model, Cmd msg )
pasteToClipboard model =
    let
        newEditor =
            Editor.insert (Editor.getWrapOption model.editor) (Editor.getCursor model.editor) model.clipboard model.editor
    in
    ( { model | editor = newEditor }, Cmd.none )


pasteToEditorClipboard : Model -> String -> ( Model, Cmd Msg )
pasteToEditorClipboard model str =
    let
        cursor =
            Editor.getCursor model.editor

        editor2 =
            Editor.placeInClipboard str model.editor
    in
    { model | editor = Editor.insert (Editor.getWrapOption model.editor) cursor str editor2 }
        |> withCmd rerender


rerender : Cmd Msg
rerender =
    Task.perform Rerender Time.now



-- UPDATE HELPERS


syncAndHighlightRenderedText : String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
syncAndHighlightRenderedText str cmd model =
    case model.renderingData of
        MD data ->
            mdSyncAndHighlightRenderedText data str cmd model

        ML _ ->
            ( model, cmd )


mdSyncAndHighlightRenderedText : MDData Msg -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
mdSyncAndHighlightRenderedText mdData str cmd model =
    let
        ( _, id0 ) =
            Parse.getId (String.trim str) (Parse.sourceMap mdData.fullAst)

        id1 =
            Maybe.withDefault "i0v0" id0

        id2 =
            Parse.idFromString id1 |> (\( id_, version ) -> ( id_, version + 1 ))

        newCounter =
            model.counter + 1

        id3 =
            id2 |> (\( id_, _ ) -> ( id_, newCounter ))

        sourceText =
            Maybe.map .content model.currentDocument |> Maybe.withDefault "empty source"
    in
    ( mdProcessContentForHighlighting mdData sourceText { model | selectedId = id3, counter = newCounter }
    , Cmd.batch [ cmd, setViewportForElement (Parse.stringFromId id3) ]
    )


mdProcessContentForHighlighting : MDData Msg -> String -> Model -> Model
mdProcessContentForHighlighting mdData str model =
    let
        newAst_ =
            Parse.toMDBlockTree model.counter ExtendedMath str

        newAst =
            Diff.mergeWith Parse.equalIds mdData.fullAst newAst_

        renderedText =
            Markdown.ElmWithId.renderHtmlWithExternaTOC model.selectedId "Contents" newAst

        newMDData =
            { mdData | renderedText = renderedText }
    in
    { model | renderingData = MD newMDData }


syncWithEditor : Model -> Editor.Editor -> Cmd EditorMsg -> ( Model, Cmd Msg )
syncWithEditor model newEditor editorCmd =
    { model | editor = newEditor }
        |> Update.Document.text (Preprocessor.apply (Editor.getSource newEditor))
        |> withCmd (editorCmd |> Cmd.map EditorMsg)


updateEditor : Model -> Editor.Editor -> Cmd EditorMsg -> ( Model, Cmd Msg )
updateEditor model newEditor editorCmd =
    ( { model | editor = newEditor }, editorCmd |> Cmd.map EditorMsg )


syncRenderedText : String -> Model -> Cmd Msg
syncRenderedText str model =
    let
        id =
            case model.renderingData of
                MD data ->
                    case Sync.getId (String.trim str) data.sourceMap of
                        Nothing ->
                            "666"

                        Just id_ ->
                            id_

                ML data ->
                    "???"
    in
    setViewportForElement id


setViewportForElement : String -> Cmd Msg
setViewportForElement id =
    Dom.getViewportOf "__rt_scroll__"
        |> Task.andThen (\vp -> getElementWithViewPort vp id)
        |> Task.attempt SetViewPortForElement


getElementWithViewPort : Dom.Viewport -> String -> Task Dom.Error ( Dom.Element, Dom.Viewport )
getElementWithViewPort vp id =
    Dom.getElement id
        |> Task.map (\el -> ( el, vp ))


setViewPortForSelectedLine : Dom.Element -> Dom.Viewport -> Cmd Msg
setViewPortForSelectedLine element viewport =
    let
        y =
            viewport.viewport.y + element.element.y - element.element.height - 100
    in
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf "__rt_scroll__" 0 y)



-- NAVIGATION HELPERS --


{-| focusOnId is used to load a subdocuemnt when the user licks on it itn the table of contents
-}
focusOnId : Model -> Uuid -> ( Model, Cmd Msg )
focusOnId model id =
    case model.tocData of
        Nothing ->
            ( model, Cmd.none |> Cmd.map TOC )

        Just zipper ->
            let
                currentDocument =
                    List.filter (\item -> item.id == id) model.tableOfContents |> List.head

                ( newModel, cmd ) =
                    case currentDocument of
                        Nothing ->
                            ( model, Cmd.none )

                        Just document ->
                            Update.Document.render model document
            in
            ( { newModel
                | currentDocument = currentDocument
                , tocData = Just (TocZ.focus id zipper)
                , tocCursor = Just id
              }
                |> Update.Tool.setupToEdit
            , cmd
            )


handleLink : Model -> String -> ( Model, Cmd Msg )
handleLink model link =
    case AppNavigation.classify link of
        ( TocRef, id_ ) ->
            ( model, Cmd.Document.scrollIfNeeded id_ )

        ( DocRef, slug ) ->
            ( model, Cmd.Document.getBySlug hasuraToken slug )

        ( IdRef, idRef ) ->
            ( model, Cmd.Document.getById hasuraToken idRef )

        ( SubdocIdRef, idRef ) ->
            focusOnId model (idRef |> Uuid.fromString |> Maybe.withDefault Utility.id0)



-- TIME HELPERS


handleTime : Model -> Posix -> ( Model, Cmd Msg )
handleTime model newTime =
    let
        flashCounterForTotalWordCount =
            if model.flashCounterForTotalWordCount > 0 then
                model.flashCounterForTotalWordCount - 1

            else
                0

        flashCounterForShareUrl =
            if model.flashCounterForShareUrl > 0 then
                model.flashCounterForShareUrl - 1

            else
                0

        secondsWhileDirty =
            if model.currentDocumentDirty then
                model.secondsWhileDirty + 1

            else
                model.secondsWhileDirty

        cmd =
            if model.secondsWhileDirty > 4 then
                case ( model.currentUser, model.currentDocument ) of
                    ( Nothing, _ ) ->
                        Cmd.none

                    ( _, Nothing ) ->
                        Cmd.none

                    ( Just user, Just document ) ->
                        Request.updateDocument hasuraToken user.username document |> Cmd.map Req

            else
                Cmd.none
    in
    ( { model
        | time = newTime
        , secondsWhileDirty = secondsWhileDirty
        , flashCounterForTotalWordCount = flashCounterForTotalWordCount
        , flashCounterForShareUrl = flashCounterForShareUrl
      }
    , cmd
    )



-- USER HELPERS


addUserPermission : Model -> ( Model, Cmd Msg )
addUserPermission model =
    case ( model.currentUser, model.currentDocument ) of
        ( Nothing, _ ) ->
            ( model, Cmd.none )

        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( Just user, Just document ) ->
            let
                updatedPermissions =
                    Document.addPermission model.usernameToAddToPermmission model.permissionToAdd document.permissions

                updatedDocument =
                    { document | permissions = updatedPermissions }

                newDocumentList =
                    Document.replaceInList updatedDocument model.documentList
            in
            ( { model
                | currentDocument = Just updatedDocument
                , documentList = newDocumentList
              }
            , Request.updateDocument hasuraToken user.username updatedDocument |> Cmd.map Req
            )


signIn : Model -> ( Model, Cmd Msg )
signIn model =
    ( { model
        | currentUser = Nothing
        , token = Nothing
        , visibilityOfTools = Invisible
        , searchMode = UserSearch
        , message = ( AuthMessage, "Signing in ..." )
      }
    , Request.signInUser model.username model.password |> Cmd.map Req
    )



-- DOCUMENT HELPERS --


loadDocument : Model -> Document -> ( Model, Cmd Msg )
loadDocument model document =
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
        |> Update.Tool.setupToEdit
    , cmd
    )


reloadDocument : Model -> Document -> ( Model, Cmd Msg )
reloadDocument model document =
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
        , documentListDisplay = ( SearchResults, DequeViewOff )
        , docType = Document.getDocType (Just document)
        , message = ( UserMessage, "Success loading document" )
      }
        |> Update.Tool.setupToEdit
    , cmd
    )


deleteDocument : Model -> ( Model, Cmd Msg )
deleteDocument model =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just document ->
            case model.documentDeleteState of
                SafetyOn ->
                    ( { model | message = ( UserMessage, "Turning safety off.  Press again to delete document." ), documentDeleteState = Armed }
                    , Cmd.none
                    )

                Armed ->
                    let
                        user =
                            model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")

                        newDeque =
                            BoundedDeque.filter (\doc -> doc.id /= document.id) model.deque
                    in
                    ( { model
                        | message = ( UserMessage, "Deleting document ..." )
                        , documentDeleteState = SafetyOn
                        , deque = newDeque
                      }
                    , Cmd.batch
                        [ Request.deleteDocument hasuraToken user.username document |> Cmd.map Req
                        , Cmd.Document.sendDequeOutside newDeque
                        ]
                    )


setDocumentPublic : Model -> Bool -> ( Model, Cmd Msg )
setDocumentPublic model bit =
    case ( model.currentDocument, model.currentUser ) of
        ( Nothing, _ ) ->
            ( model, Cmd.none )

        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( Just document, Just user ) ->
            if document.authorIdentifier /= user.username then
                ( model, Cmd.none )

            else
                let
                    newDocument =
                        { document | public = bit }
                in
                ( { model
                    | currentDocument = Just newDocument
                    , currentDocumentDirty = True
                    , documentList = Document.replaceInList newDocument model.documentList
                  }
                , Request.updateDocument hasuraToken user.username newDocument |> Cmd.map Req
                )


processTagString : Model -> String -> ( Model, Cmd Msg )
processTagString model str =
    let
        ( newDocumentList, newCurrentDocument ) =
            case model.currentDocument of
                Nothing ->
                    ( model.documentList, Nothing )

                Just doc ->
                    let
                        newDoc =
                            { doc | tags = str |> String.split "," |> List.map (String.toLower >> String.trim) }
                    in
                    ( Document.replaceInList newDoc model.documentList, Just newDoc )
    in
    ( { model
        | tagString = str
        , currentDocumentDirty = True
        , secondsWhileDirty = 0
        , currentDocument = newCurrentDocument
        , documentList = newDocumentList
      }
    , Cmd.none
    )


getTagString : Maybe Document -> String
getTagString maybeDocument =
    case maybeDocument of
        Nothing ->
            ""

        Just document ->
            document.tags |> String.join ", "



-- MANAGE DOCUMENTS --


handleDeletedDocument model =
    case model.currentDocument of
        Nothing ->
            ( { model | message = ( DebugMessage, "Odd, something weird happened in deleting your document" ) }, Cmd.none )

        Just deletedDocument ->
            let
                newDocumentList =
                    List.filter (\doc -> doc.id /= deletedDocument.id) model.documentList

                newDocument =
                    List.head newDocumentList

                docType =
                    Document.getDocType newDocument

                newDocumentText =
                    Maybe.map .content (List.head newDocumentList) |> Maybe.withDefault "This is test"

                ( renderingData, cmd ) =
                    Update.Render.prepare model newDocument
            in
            ( { model
                | documentList = newDocumentList
                , currentDocument = List.head newDocumentList
                , renderingData = renderingData
                , message = ( UserMessage, "Document " ++ deletedDocument.title ++ " deleted" )
                , visibilityOfTools = Invisible
              }
            , cmd
            )



--
-- VIEW FUNCTIONS
--


view : Model -> Browser.Document Msg
view model =
    case model.appMode of
        Reading ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (View.Reader.view viewInfoReading model)
                |> documentMsgFromHtmlMsg "Reading"

        Editing StandardEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (View.Editor.view viewInfoEditing model)
                |> documentMsgFromHtmlMsg "Editing"

        Editing SubdocumentEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (View.Editor.viewSubdocuments viewInfoEditingSubdocuemnt model)
                |> documentMsgFromHtmlMsg "Edit Subdocuments"

        UserMode _ ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (View.User.view viewInfoUserPage model)
                |> documentMsgFromHtmlMsg "User Mode"


documentMsgFromHtmlMsg : String -> Html msg -> Browser.Document msg
documentMsgFromHtmlMsg title msg =
    { title = title
    , body = [ msg ]
    }



-- layoutWith {options = [focusStyle Focusstyle] }
{-

   and there is also the hack of conditionally add
    html <| layoutWith {options}

-}


myFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }
