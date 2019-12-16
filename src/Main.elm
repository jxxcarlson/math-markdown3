module Main exposing (main)

import Model exposing
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
import Browser
import String.Interpolate exposing(interpolate)
import Interchange
import Debounce
import File exposing (File)
import File.Select as Select
import Utility.Time
import AppNavigation exposing(NavigationType(..))
import Render.Types exposing (RenderedText)
import Editor
import Button
import Views.MarkdownEditor exposing (editor, onBlur, onChanged, onFocus, value)
import Update.UI
import KeyboardManager
import Browser.Events
import Browser.Navigation as Nav
import CustomElement.CodeEditor as CodeEditor
import Data
import Update.Master
import Cmd.Document
import Utility
import Utility.View
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Element exposing (..)
import Element.Background as Background
import Update.Render
import Element.Border as Border
import Config
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import BoundedDeque exposing(BoundedDeque)
import Element.Lazy
import Html exposing (..)
import Html.Attributes as HA
import Json.Encode as E
import Keyboard exposing (Key(..))
import Document
import Outside
import Markdown.Elm
import Markdown.Option as MDOption
import Markdown.ElmWithId
import Markdown.Option exposing (Option(..))
import ParseWithId
import Preprocessor
import Prng.Uuid as Uuid exposing (Uuid)
import Random
import Search
import Render exposing(RenderingOption(..))
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData(..))
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..), orderByMostRecentFirst, orderByTitleAsc)
import Style
import Task exposing (Task)
import Time exposing (Posix)
import TocManager
import TocZ exposing (TocMsg(..), viewZ)
import Update.Document
import Url exposing(Url)
import User exposing (AuthorizedUser, User)
import Utility



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


type alias ViewInfo =
    { toolStripWidth : Float
    , docListWidth : Float
    , editorWidth : Float
    , renderedDisplayWidth : Float
    , tocWidth : Float
    , vInset : Float
    , hExtra : Float
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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator (initialSeed flags.seed flags.randInts)
        initialAst = Markdown.ElmWithId.parse -1 ExtendedMath Data.loadingPage.content

        model : Model
        model =
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
            , documentListDisplay = (SearchResults, DequeViewOff)
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

            , renderingData = Render.load -1 (OMarkdown MDOption.Standard) "Empty document"



            , tagString = ""
            , searchTerms = ""
            , sortTerm = orderByMostRecentFirst
            , searchMode = PublicSearch
            , sortMode = MostRecentFirst
            , documentOutline = ""
            , usernameToAddToPermmission = ""
            , permissionToAdd = NoPermission
            }
    in
    ( model
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
       initialAst = Markdown.ElmWithId.parse -1 ExtendedMath Data.loadingPage.content
   in
        { model |

        -- UI
         docType = Markdown MDExtendedMath

        , visibilityOfTools = Invisible
        , appMode = UserMode SignInState
        , documentListDisplay = (SearchResults, DequeViewOff)
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
        , renderingData = Render.load -1 (OMarkdown MDOption.Standard) "Empty document"
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
        [ Time.every 1000 Tick
        , Browser.Events.onResize WindowSize
        , Sub.map KeyMsg Keyboard.subscriptions
        , Outside.getInfo Outside LogErr
        ]



-- UPDATE FUNCTION

textTask : String -> Cmd Msg
textTask str =
    Task.perform UpdateDocumentText (Task.succeed str)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FeedDebouncer str ->
              let
                -- Push your values here.
                (debounce, cmd) =
                  Debounce.push debounceConfig str model.debounce
              in
                ( { model
                    | -- value = s
                     debounce = debounce
                  }
                , cmd
                )

        DebounceMsg debounceMsg ->
              let
                (debounce, cmd) =
                  Debounce.update
                    debounceConfig
                    (Debounce.takeLast textTask)
                    debounceMsg
                    model.debounce
              in
                ( { model | debounce = debounce }
                , cmd
                )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        -- PORTS --

        Outside infoForElm ->
            case infoForElm of
                Outside.UserDataFromOutside outsideUser ->
                    let
                        user = User.fromOutside outsideUser
                    in
                      ({model | currentUser = Just user
                                 ,appMode = Reading
                                  , documentListDisplay = (SearchResults, DequeViewOn)
                                  , focusedElement = NoFocus
                                  , visibilityOfTools = Invisible
                                  , searchMode = UserSearch
                                , token =  Just outsideUser.token}
                            , Cmd.Document.getUserDocumentsAtSignIn                                                                                                                                                                                user)

                Outside.GotSelection selection ->
                    let
                        maybeLineNumber = case model.currentDocument of
                           Nothing -> Nothing
                           Just doc -> Editor.lineNumber (String.left 16 selection) doc.content
                        (message, cmd) = case maybeLineNumber of
                            Just k -> ("Line number: "  ++ String.fromInt k,  Outside.sendInfo (Outside.ScrollToLine (E.int k)))
                            Nothing -> ("Could not find line", Cmd.none)
                    in
                    ({model | editorTargetLineNumber = maybeLineNumber, selectedText = selection, message = (UserMessage, message)}
                      , cmd)

                Outside.UuidList uuidList ->
                    (model, Request.documentsInIdList hasuraToken uuidList GotDequeDocuments |> Cmd.map Req)

        LogErr err ->
            ({model | message = (ErrorMessage, err)}, Cmd.none)


        Model.Clear ->    ( { model
                | counter = model.counter + 1
              }
            , Cmd.none
            )

        SetDocumentListType documentListType ->
            let
                dv = Tuple.second model.documentListDisplay
            in
            case documentListType of
                SearchResults  ->
                    ( { model | documentListDisplay = (SearchResults, DequeViewOff) }, Cmd.none )

                DocumentChildren ->
                    ( { model | documentListDisplay = (DocumentChildren, DequeViewOff) }, Cmd.none )

        SetDequeview dv ->
                    let
                        slt = Tuple.first model.documentListDisplay
                    in
                    ( { model | documentListDisplay = (slt, dv) }, Cmd.none )

        ToggleDequeview ->
           let
              newDocumentListDisplay = case model.documentListDisplay of
                (slt, DequeViewOn) -> (slt, DequeViewOff)
                (slt, DequeViewOff) -> (slt, DequeViewOn)

           in
             ({model | documentListDisplay = newDocumentListDisplay}, Cmd.none)

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
            let
                currentDocument =
                    case model.currentDocument of
                        Nothing ->
                            Nothing

                        Just doc ->
                            Just { doc | docType = docType }
            in
            ( { model
                | docType = docType
                , currentDocument = currentDocument
              }
            , Cmd.none
            )

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
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

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
            ( bareModel model ,  Outside.sendInfo (Outside.DestroyUserData E.null)
            )


        -- EDITOR --
        ProcessLine str ->
            let
                id =
                    (case Editor.findStringInAST str model.renderingData of
                        Nothing ->
                            "??"

                        Just id_ ->
                            id_ |> ParseWithId.stringOfId)
            in
            ( { model | message = ( UserMessage, "str = " ++ String.left 20 str ++ " -- Clicked on id: " ++ id ) }
            , Cmd.Document.setViewportForElement id
            )

        SyncEditorToLine k ->
            (model, Cmd.none)

        SetViewPortForElement result ->
            case result of
                Ok ( element, viewport )  ->
                    ( model, Cmd.Document.setViewPortForSelectedLine element viewport )

                Err _ ->
                   ({ model | message =  (ErrorMessage, (Tuple.second model.message) ++ ", doc VP ERROR") }, Cmd.none )

        GetTextSelection ->
            (model, Outside.sendInfo (Outside.GetTextSelectionFromOutside E.null))

        -- DOCUMENT --
        DownloadArchive ->
             Update.Document.downloadArchive model

        ArchiveRequested ->
            ( model, Select.file ["application/json"] ArchiveSelected )

        ArchiveSelected file ->
            (model, Task.perform ArchiveLoaded (File.toString file))

        ArchiveLoaded archiveString ->
            let
                importedDocuments = Interchange.decodeDocumentList archiveString |> Maybe.withDefault []
                author = List.map .authorIdentifier importedDocuments  |> List.head |> Maybe.withDefault "NoAuthor"
                message =
                  ( UserMessage, "Imported docs:  " ++ String.fromInt (List.length importedDocuments) ++ " (by " ++ author ++ ")")
            in
            case List.length importedDocuments > 0 of
                False -> ( { model | message = message}, Cmd.none )
                True ->
                  let
                    (renderingData, cmd) = Update.Render.prepare model (List.head importedDocuments)
                  in
                    ( { model | documentList = importedDocuments
                         , currentDocument = List.head importedDocuments
                         , renderingData = renderingData
                         , counter = model.counter + 2
                         ,  message = message}
                     , cmd )

        SaveImportedArchive ->
            let
               insertDoc = (\doc -> Request.insertDocument Config.hasuraToken doc |> Cmd.map Req)
               cmdList = List.map insertDoc model.documentList
            in
              (model, Cmd.batch cmdList)


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
            ( { model | renderingData = rd  }, Cmd.none )

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
            Update.Document.text model (Preprocessor.apply str)

        SetCurrentDocument document ->
             Update.Document.setCurrent model document (Cmd.Document.sendDequeOutside  model.deque)


        SetCurrentSubDocument document tocItem ->
            Update.Document.setCurrentSubdocument model document tocItem

        SetUpOutline ->
            ( Update.Master.setupOutline model, Cmd.none )

        AddUserNameForPermissions str ->
            ( { model | usernameToAddToPermmission = str}, Cmd.none)


        CyclePermission  ->
           let
             nextPermission = case model.permissionToAdd of
                 NoPermission -> ReadPermission
                 ReadPermission -> WritePermission
                 WritePermission -> NoPermission
           in
            ( { model | permissionToAdd = nextPermission}, Cmd.none)



        AddUserPermission ->
            let
                _ = "ADD clicked"
            in
            addUserPermission model

        UpdateChildren ->
            case List.head model.tableOfContents of
                Nothing ->
                    ( model, Cmd.none )

                Just masterDocument ->
                    case TocManager.updateMasterAndDocumentListFromOutline model.documentOutline model.tableOfContents of
                        Err error ->
                            ( {model | message = (ErrorMessage, Document.stringOfError   error)}, Cmd.none )

                        Ok ( newMasterDocument, newDocumentList ) ->
                            let
                              user = model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")
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
             ( { model |  flashCounterForShareUrl = config.maxFlashCount }, Cmd.none )

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
                    ( { model | toggleToc = not model.toggleToc }, Cmd.batch [Cmd.Document.resetViewportOfRenderedText, Cmd.Document.resetViewportOfRenderedText])  -- Cmd.none |> Cmd.map TOC )


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
               id = String.replace "%20" " "( Maybe.withDefault "foo"  url.fragment)

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
                               _ = "SETTING UO DEQUE"
                               newDeque = BoundedDeque.fromList config.dequeLength documentList
                               currentUser = if BoundedDeque.isEmpty  newDeque then
                                                 model.currentUser
                                              else
                                                 Update.Document.updateMaybeUserWithDeque newDeque model.currentUser
                               cmd = case BoundedDeque.isEmpty  newDeque of
                                   False -> Cmd.none
                                   True ->
                                     let
                                        docIds  = case model.currentUser of
                                                      Nothing -> []
                                                      Just user -> user.recentDocs
                                     in
                                       Request.documentsInIdList hasuraToken docIds GotDocumentsForDeque |> Cmd.map Req

                           in
                            ({model | deque = newDeque, currentUser = currentUser}, cmd)

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
                               Nothing -> ( { model| message = (UserMessage, "Couldn't load linked document")}, Cmd.none)
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
                               newDeque = BoundedDeque.fromList config.dequeLength documentList
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
                                       tokenCmd  = case model.token of
                                          Nothing -> Cmd.none
                                          Just token -> Outside.sendInfo (Outside.UserData <| User.outsideUserEncoder (User.outsideUserWithToken token user))

                                       dequeCommand = Request.documentsInIdList hasuraToken user.recentDocs GotDocumentsForDeque |> Cmd.map Req
                                   in
                                    ( { model
                                        | message = ( UserMessage, "User signup successful (2)" )
                                        , currentUser = Just user
                                        , appMode = Reading
                                        , documentListDisplay = (SearchResults, DequeViewOn)
                                        , focusedElement = NoFocus
                                        , visibilityOfTools = Invisible
                                      }
                                    ,
                                    Cmd.batch [
                                      Cmd.Document.getUserDocumentsAtSignIn  user
                                      , Cmd.batch[tokenCmd, dequeCommand]
                                      ]
                                    )



-- NAVIGATION HELPERS --
-- XYXY

focusOnId : Model -> Uuid -> (Model, Cmd Msg)
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
            , cmd
            )

handleLink : Model -> String -> (Model, Cmd Msg)
handleLink model link =
    case  AppNavigation.classify link of
        (TocRef, id_) -> (model, Cmd.Document.scrollIfNeeded id_)
        (DocRef, slug)  ->(model, Cmd.Document.getBySlug hasuraToken slug)
        (IdRef, idRef)  -> (model, Cmd.Document.getById hasuraToken idRef)
        (SubdocIdRef, idRef)  -> focusOnId model (idRef |> Uuid.fromString |> Maybe.withDefault Utility.id0)


--

-- STRINGS


appModeAsString : Model -> String
appModeAsString model =
    case model.appMode of
        Reading ->
            "Reading"

        Editing StandardEditing ->
            "Editing"

        Editing SubdocumentEditing ->
            "Editing subdocuments"

        UserMode SignInState ->
            "U, Signing in"

        UserMode SignUpState ->
            "U, Signing up"

        UserMode ChangePasswordState ->
            "U, Changing Password"

        UserMode SignedInState ->
            "U, Signed in"



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
    ( { model | time = newTime
       , secondsWhileDirty = secondsWhileDirty
       , flashCounterForTotalWordCount = flashCounterForTotalWordCount
       , flashCounterForShareUrl = flashCounterForShareUrl}
    , cmd
    )







-- USER HELPERS

addUserPermission : Model -> (Model, Cmd Msg)
addUserPermission model =
    case (model.currentUser, model.currentDocument) of
        (Nothing, _) -> (model, Cmd.none)
        (_, Nothing) -> (model, Cmd.none)
        (Just user, Just document) ->
            let

               updatedPermissions = Document.addPermission model.usernameToAddToPermmission model.permissionToAdd  document.permissions

               updatedDocument = {document | permissions = updatedPermissions }

               newDocumentList = Document.replaceInList updatedDocument model.documentList

            in
                  ( {model | currentDocument = Just updatedDocument
                     , documentList = newDocumentList
                    }
                   , Request.updateDocument hasuraToken user.username updatedDocument |> Cmd.map Req)



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


-- SEARCH HELPERS



-- DOCUMENT HELPERS --






loadDocument : Model -> Document -> ( Model, Cmd Msg )
loadDocument model document =
    let
        (renderingData, cmd) = Update.Render.prepare model (Just document)

    in
    ( { model
        | documentList = document :: (List.filter (\doc -> doc.id /= document.id) model.documentList)
        , currentDocument = Just document
        , tagString = getTagString (Just document)
        , counter = model.counter + 2
        , renderingData = renderingData
        , appMode = Reading
        , documentListDisplay = (SearchResults, DequeViewOff)
        , docType = Document.getDocType (Just document)
        , message = ( UserMessage, "Success loading document" )
      }
    , cmd
    )


--updateDeque : Model -> Document -> (BoundedDeque Document, Cmd Msg)
--updateDeque model document =
--    let
--        newDeque = Document.pushFrontUnique document model.deque
--    in
--    case model.currentUser of
--          Nothing -> (model.deque, Cmd.none)
--          Just user -> (newDeque, Request.updateUser hasuraToken (updateUserWithDeque newDeque user) |> Cmd.map Req)








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
                     user = model.currentUser |> Maybe.withDefault (User.dummy "_nobody_")
                     newDeque = BoundedDeque.filter (\doc -> doc.id /= document.id) model.deque
                   in
                    ( { model | message = ( UserMessage, "Deleting document ..." )
                       , documentDeleteState = SafetyOn
                       , deque = newDeque}
                    , Cmd.batch [
                         Request.deleteDocument hasuraToken user.username document |> Cmd.map Req
                         , Cmd.Document.sendDequeOutside  newDeque
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



-- PARSE AND RENDER





-- UI HELPERS



setElementId : String -> Element.Attribute msg
setElementId id =
   Element.htmlAttribute <| HA.attribute "id" id

setHtmlId : String -> Html.Attribute msg
setHtmlId id =
    HA.attribute "id" id

scale : Float -> Int -> Int
scale factor input =
    factor * toFloat input |> round


affine : Float -> Float -> Int -> Int
affine factor shift input =
    factor * (toFloat input - shift) |> round


translate : Float -> Int -> Int
translate amount input =
    toFloat input + amount |> round


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


                (renderingData, cmd) = Update.Render.prepare model newDocument

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
---

documentMsgFromHtmlMsg : String -> Html msg -> Browser.Document msg
documentMsgFromHtmlMsg title msg  =
    { title = title
    , body = [msg] }

type alias RenderedDocumentRecord msg =
    { document : Html msg, title : Html msg, toc : Html msg }




view : Model -> Browser.Document Msg
view model =
    case model.appMode of
        Reading ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (readerView viewInfoReading model)
               |> documentMsgFromHtmlMsg "Reading"


        Editing StandardEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (editorView viewInfoEditing model)
               |> documentMsgFromHtmlMsg "Editing"

        Editing SubdocumentEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (subdocumentEditorView viewInfoEditingSubdocuemnt model)
               |> documentMsgFromHtmlMsg "Edit Subdocuments"

        UserMode _ ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (userView viewInfoUserPage model)
               |> documentMsgFromHtmlMsg "User Mode"



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



-- USER PAGE --


type alias ViewInfoUserPage =
    { lhsFraction : Float, rhsFraction : Float, vInset : Float }


userView : ViewInfoUserPage -> Model -> Element Msg
userView viewInfo model =
    let
        h_ =
            translate -viewInfo.vInset model.windowHeight
    in
    column [ Background.color (Style.makeGrey 0.35), Font.color Style.white ]
        [ userPageHeader viewInfo model
        , row [ height (px h_), Font.size 14 ] [ lhsViewInfoPage viewInfo model, rhsViewInfoPage viewInfo model ]
        , userPageFooter model
        ]


lhsViewInfoPage viewInfo model =
    let
        w =
            scale viewInfo.lhsFraction model.windowWidth

        h =
            translate -viewInfo.vInset model.windowHeight
    in
    column [ width (px w), height (px h), padding 12, spacing 24 ]
        [ if model.appMode == UserMode SignInState then
            signInUpView model

          else
            case model.currentUser of
                Nothing ->
                    signInUpView model

                Just user ->
                    signedInUserView model user
        ]


rhsViewInfoPage viewInfo model =
    let
        w1 =
            scale (0.7 * viewInfo.rhsFraction) model.windowWidth

        w2 =
            scale (0.3 * viewInfo.rhsFraction) model.windowWidth

        h =
            translate -viewInfo.vInset model.windowHeight

        rt : { title : Html msg, toc : Html msg, document : Html msg }
        rt =
            Markdown.Elm.toHtmlWithExternaTOC ExtendedMath Data.rhsUserText
    in
    row []
        [ column [] [
            column [] [ rt.title |> Element.html]
          , column [ width (px w1), height (px h), padding 36, scrollbarY, Background.color (Style.makeGrey 0.9), Font.color (Style.makeGrey 0.1) ]
               [ rt.document |> Element.html ]
           ]
        , column [ width (px w2), height (px h), padding 12, scrollbarY, Background.color (Style.makeGrey 0.8), Font.color (Style.makeGrey 0.1) ]
            [ rt.toc |> Element.html ]
        ]




userPageFooter : Model -> Element Msg
userPageFooter model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ el [] (Element.text <| appModeAsString model)
        ]


authMessageDisplay : Model -> Element Msg
authMessageDisplay model =
    case model.message of
        ( AuthMessage, str ) ->
            el [ Font.color Style.white, Font.size 18 ] (Element.text str)

        _ ->
            Element.none


userPageHeader : ViewInfoUserPage -> Model -> Element Msg
userPageHeader viewInfo model =
    let
        lhsWidth =
            scale viewInfo.lhsFraction model.windowWidth

        rhsWidth =
            scale viewInfo.rhsFraction model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ modeButtonStrip model lhsWidth
        , column [ width (px rhsWidth) ] []
        ]


modeButtonStrip model lhWidth =
    row [ width (px lhWidth), height (px 45), spacing 10, paddingXY 20 0 ]
        [ editTools model
        , Button.readingMode model
        , Button.userPageMode model
        ]



-- SIGN-IN-UP-OUT


signInUpView : Model -> Element Msg
signInUpView model =
    column Style.signInColumn
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (Element.text "Sign in/up/out")
        , column [ spacing 8, paddingXY 0 18 ]
            [ el [ Font.size 14 ] (Element.text "This project is a work-in-progress.")
            , el [ Font.size 14 ] (Element.text "Comments to jxxcarlson on the Elm Slack")
            , el [ Font.size 14 ] (Element.text "or to jxxcarlson at gmail")
            ]
        , outerPasswordPanel model
        ]


outerPasswordPanel : Model -> Element Msg
outerPasswordPanel model =
    column [ spacing 24 ]
        [ inputUserName model
        , inputPassword model
        , Utility.View.showIf (model.appMode == UserMode SignUpState) (inputPasswordConfirmation model)
        , Utility.View.showIf (model.appMode == UserMode SignUpState) (inputEmail model)
        , Utility.View.showIf (model.appMode == UserMode SignUpState) (el [ Font.size 12 ] (Element.text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ Utility.View.showIf (model.appMode == UserMode SignInState || model.currentUser == Nothing) Button.signIn
            , row [ spacing 12 ]
                [ Button.signUp model
                , Utility.View.showIf (model.appMode == UserMode SignUpState) (Button.cancelSignUp model)

                ]
            ]
        , authMessageDisplay model
        , el [ Font.size 16, Font.color Style.white ] (Element.text (signInMessage model.message))
        ]


signInMessage : Message -> String
signInMessage ( messageType, messageContent_ ) =
    if messageType == SignInMessage then
        messageContent_

    else
        ""


signedInUserView : Model -> User -> Element Msg
signedInUserView model user =
    column Style.signInColumn
        [ el [] (Element.text <| "Signed in as " ++ user.username)
        , Button.signOut model
        , Utility.View.showIf (model.appMode == UserMode ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ Button.changePassword model
            , Utility.View.showIf (model.appMode == UserMode ChangePasswordState) (Button.cancelChangePassword model)
            ]
        , adminStatus model
        ]


passwordPanel : Model -> Element Msg
passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 16, Font.color Style.darkRed ] (Element.text (signInMessage model.message))
        ]


inputCurrentPassword : Model -> Element Msg
inputCurrentPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (Element.text "Old password: ")
        }


inputNewPassword1 : Model -> Element Msg
inputNewPassword1 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword1
        , show = False
        , text = model.newPassword1
        , placeholder = Nothing

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (Element.text "New password: ")
        }


inputNewPassword2 : Model -> Element Msg
inputNewPassword2 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword2
        , text = model.newPassword2
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (Element.text "Password again: ")
        }



adminStatus : Model -> Element Msg
adminStatus model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case user.admin of
                False ->
                    Element.none

                True ->
                    el [ Font.size 12 ] (Element.text "Admin")


inputUserName : Model -> Element Msg
inputUserName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotUserName
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Username")
        }


inputEmail : Model -> Element Msg
inputEmail model =
    Input.text (Style.inputStyle 200)
        { onChange = GotEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Email")
        }


inputPassword : Model -> Element Msg
inputPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100), height (px 25) ] (Element.text "Password")
        }


inputPasswordConfirmation : Model -> Element Msg
inputPasswordConfirmation model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPasswordConfirmation
        , text = model.passwordConfirmation
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Password (2)")
        }

-- SUBDOCUMENT EDITOR


subdocumentEditorView : ViewInfo -> Model -> Element Msg
subdocumentEditorView viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "--"
    in
    column []
        [ simpleEditingHeader viewInfo model
        , row []
            [ tabStrip viewInfo model
            , toolsOrDocs viewInfo model
            , subDocumentTools model
            , column [ spacing 12, alignTop, padding 20 ]
                [ row [ spacing 8 ] [ el [ Font.size 14 ] (Element.text "Edit outline below"), Button.setupOutline model, Button.updateChildren model ]
                , inputOutline model
                ]
            ]
        , footer model
        ]


subDocumentTools model =
    let
        ( message1, message2 ) =
            case model.currentDocument of
                Nothing ->
                    ( "Master document not selected", "" )

                Just master ->
                    ( "Search, then click below to add subdocument to", master.title )
    in
    column [ spacing 12, paddingXY 18 24, alignTop ]
        [ el [ Font.size 14, width (px 300) ] (Element.text message1)
        , el [ Font.size 14, width (px 300), Font.bold ] (Element.text message2)
        , column [ Font.size 13, spacing 8, width (px 350), height (px 500), Border.color Style.charcoal, Border.width 1, padding 12, scrollbarY ]
            (List.map Button.addSubdocument2 model.candidateChildDocumentList)
        ]




getTitles : List Document -> String
getTitles docList =
    List.map .title docList
        |> String.join "\n"


inputOutline model =
    Input.multiline (Style.textInputStyleSimple 300 500)
        { onChange = GotOutline
        , text = model.documentOutline
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "")
        , spellcheck = False
        }



-- TEXT EDITOR


editorView : ViewInfo -> Model -> Element Msg
editorView viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "---"

        rt : RenderedText Msg
        rt =
            Render.get model.renderingData

        newViewInfo = {viewInfo | docListWidth = viewInfo.docListWidth +  0.1* viewInfo.editorWidth
                        , editorWidth = viewInfo.editorWidth + viewInfo.tocWidth / 2 -  0.1* viewInfo.editorWidth
                        , renderedDisplayWidth = viewInfo.renderedDisplayWidth + viewInfo.tocWidth / 2
                        , tocWidth = 0}
    in
    column []
        [ editingHeader newViewInfo model rt
        , row [] [ tabStrip newViewInfo model
          , toolsOrDocs newViewInfo model
          , editor newViewInfo model
          , Element.Lazy.lazy (renderedSource newViewInfo model footerText) rt ]
        , footer model
        ]



-- READER


readerView : ViewInfo -> Model -> Element Msg
readerView viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "---"

        rt : RenderedText Msg
        rt =
           Render.get model.renderingData
    in
    column [ paddingXY 0 0 ]
        [ readingHeader viewInfo model rt
        , row []
            [ tabStrip viewInfo model
            , toolsOrDocs viewInfo model
            , Element.Lazy.lazy (renderedSource viewInfo model footerText) rt
            ]
        , footer model
        ]

-- VIEW RENDERED SOURCE --

renderedSource : ViewInfo -> Model -> String -> RenderedText Msg -> Element Msg
renderedSource viewInfo model footerText_ rt =
    let
        w_ =
            affine viewInfo.renderedDisplayWidth viewInfo.hExtra model.windowWidth

        h_ =
            translate -viewInfo.vInset model.windowHeight

        w2_ =
            affine viewInfo.renderedDisplayWidth (viewInfo.hExtra + 160) model.windowWidth

        wToc =
          affine viewInfo.tocWidth viewInfo.hExtra model.windowWidth

        hToc =
            translate -viewInfo.vInset model.windowHeight

        outerSourceStyle = [ setElementId "__rt_scroll__", width (px w_), height (px h_), clipX, Font.size 12 ]

        innerSourceStyle = [setElementId Cmd.Document.masterId,  height (px h_)]

        outerTocStyle = [ height (px hToc), width (px wToc), Font.size 12, paddingXY 8 0, Background.color (Style.makeGrey 0.9) ]

        innerTocStyle = [  height (px (hToc - 125)), scrollbarY, clipX ]

        footerStyle = [ paddingXY 12 3, width fill, height (px 125), clipX, Background.color (Style.makeGrey 0.5), Font.color (Style.makeGrey 1.0) ]

    in
    row [ spacing 10 ]
        [ column outerSourceStyle
            [ column [ width (px w2_), paddingXY 10 20 ]
                [ column innerSourceStyle [ rt.document |> Element.html ] ]
            ]
        , Element.column outerTocStyle
            [ column  innerTocStyle [ rt.toc |> Element.html ]
            , column  footerStyle
                [ renderFooter footerText_ ]
            ]
        ]


renderFooter : String -> Element Msg
renderFooter str =
    pre [ HA.style "font-size" "10px" ] [ Html.text str ] |> Element.html


toolsOrDocs viewInfo model =
    case ( model.visibilityOfTools, model.appMode ) of
        ( Visible, Editing StandardEditing ) ->
            toolPanel viewInfo model

        ( _, _ ) ->
            docListViewer viewInfo model



-- EDITOR --


{-| A wrapper for editor_, which does the real editing work. -}
editor : ViewInfo -> Model -> Element Msg
editor viewInfo model =
    let
        w =
            affine viewInfo.editorWidth viewInfo.hExtra model.windowWidth |> toFloat

        h =
            translate -viewInfo.vInset model.windowHeight |> toFloat
    in
    column []
        [ Element.Keyed.el []
            ( String.fromInt 0
            , editor_ model w h
            )
        ]
{-| Does teh real editing work.  -}
editor_1 : Model -> Float -> Float -> Element Msg
editor_1 model w h =
    let
        wpx =
            Utility.pxFromFloat w

        hpx =
            Utility.pxFromFloat h
    in
    CodeEditor.codeEditor
        [ CodeEditor.editorValue (Document.getContent model.currentDocument)
        -- , CodeEditor.onEditorChanged UpdateDocumentText -- FeedDebouncer -- Inform the editor custom element of the change in text
        , CodeEditor.onEditorChanged FeedDebouncer -- Inform the editor custom element of the change in text
        , CodeEditor.onGutterClicked ProcessLine  -- Respond to clicks by scrolling the rendered to text to the corresponding position.
        ]
        []
        |> (\x -> Html.div [ setHtmlId "_editor_",  HA.style "width" wpx, HA.style "height" hpx, HA.style "overflow" "scroll" ] [ x ])
        |> Element.html


editor_ : Model -> Float -> Float -> Element Msg
editor_ model w h =
    let
        wpx =
            Utility.pxFromFloat w

        hpx =
            Utility.pxFromFloat h
    in
    Views.MarkdownEditor.editor
        [
             value (Document.getContent model.currentDocument)
            -- , onChanged UpdateDocumentText -- FeedDebouncer -- Inform the editor custom element of the change in text
           , onChanged FeedDebouncer -- Inform the editor custom element of the change in text
           -- , CodeEditor.onGutterClicked ProcessLine  -- Respond to clicks by scrolling the rendered to text to the corresponding position.
        ]
           |> (\x -> Html.div [ setHtmlId "_editor_",  HA.style "width" wpx, HA.style "height" hpx, HA.style "overflow" "scroll" ] [ x ])
           |> Element.html




-- DOCUMENT TOOL PANEL: BUTTONS AND INPUTS --


toolPanel viewInfo model =
    let
        h_ =
            translate -viewInfo.vInset model.windowHeight
    in
    column
        [ width (px (scale (1.35*viewInfo.docListWidth) model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.5)
        , paddingXY 20 20
        , alignTop
        ]
        [ column [ Font.size 13, spacing 25 ]
            [ el [ Font.size 16, Font.bold, Font.color Style.white ] (Element.text "Document tools")
            , Button.togglePublic model
            , inputTags model
            , docTypePanel model
            , userPermissions model
            ]
        ]

userPermissions model =
    column [spacing 10, padding 10, Background.color Style.charcoal]
      [  el [Font.color Style.white] (Element.text "User permissions")
        , addUserRow model
        , viewPermissions model
      ]


viewPermissions model =
    case model.currentDocument of
        Nothing -> Element.none
        Just doc ->
          let
              permissionList = Document.listPermissions doc
          in
            column [Font.color Style.white]
              (List.map (\p -> row [] [Element.text p]) permissionList)


addUserRow model =
    row [spacing 8]
    [Button.addUserPermission, usernameToAddField model, Button.selectPermission model]


usernameToAddField model =
    Input.text (Style.inputStyle 100)
      { onChange = AddUserNameForPermissions
     , text = model.usernameToAddToPermmission
     , placeholder = Nothing
     , label = Input.labelLeft [ Font.size 14, width (px 0) ] (Element.text "")
     }






inputTags model =
    Input.multiline (Style.textInputStyleSimple 180 80)
        { onChange = GotTagString
        , text = model.tagString
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "Keywords")
        , spellcheck = False
        }





-- DOCUMENT LIST --


docListViewer viewInfo model =
    let
        h_ =
            translate -viewInfo.vInset model.windowHeight

        renderedList =
            case model.documentListDisplay of
                (SearchResults, DequeViewOff) ->
                    renderTocForSearchResults model

                (DocumentChildren, DequeViewOff) ->
                    (Button.expandCollapseToc |> Element.map TOC) :: renderTocForMaster model

                (_, DequeViewOn) ->
                    renderTocForDeque model


    in
    column
        [ width (px (scale viewInfo.docListWidth model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.9)
        , paddingXY 12 20
        , alignTop
        , clipX
        ]
        [ column [ Font.size 13, spacing 8 ]
            (heading model
                :: Button.newSubdocument model
                :: Button.deleteSubdocument model
                :: renderedList
            )
        ]



-- TABLE OF CONTENTS


renderTocForSearchResults : Model -> List (Element Msg)
renderTocForSearchResults model =
    List.map (tocEntry model.currentDocument) model.documentList


renderTocForDeque: Model -> List (Element Msg)
renderTocForDeque model =
    List.map (tocEntry model.currentDocument)
      (BoundedDeque.toList model.deque |> List.sortBy (\doc -> doc.title))

renderTocForMaster : Model -> List (Element Msg)
renderTocForMaster model =
    case model.tocData of
        Nothing ->
            [ el [] (Element.text <| "Loading TOC ...") ]

        Just zipper ->
            [ viewZ model.toggleToc zipper |> Element.map TOC ]





tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        ( color, fontWeight ) =
            tocEntryStyle currentDocument_ document
    in
    Input.button [] { onPress = Just (SetCurrentDocument document),
      label = el [ Font.color color, fontWeight ] (Element.text <| tocEntryPrefix document ++ document.title) }

tocEntryPrefix : Document -> String
tocEntryPrefix doc =
    case List.length doc.childInfo > 0 of
        True -> "+ "
        False -> ""

tocEntryStyle : Maybe Document -> Document -> ( Color, Element.Attribute msg )
tocEntryStyle currentDocument_ document =
    let
        currentDocId =
            case currentDocument_ of
                Nothing ->
                    Utility.id0

                Just doc ->
                    doc.id

        color =
            case currentDocument_ of
                Nothing ->
                    Style.buttonGrey

                Just _ ->
                    case ( currentDocId == document.id, document.childInfo /= [] ) of
                        ( True, True ) ->
                            Style.brighterBlue

                        ( True, False ) ->
                            Style.red

                        ( False, True ) ->
                            Style.brighterBlue

                        ( False, False ) ->
                            Style.grey 0.2

        fontWeight =
            case currentDocId == document.id of
                True ->
                    Font.bold

                False ->
                    Font.regular
    in
    ( color, fontWeight )






heading : Model -> Element Msg
heading model =
    let
        n_ =
            case model.documentListDisplay of
                (SearchResults, DequeViewOff) ->
                    List.length model.documentList

                (DocumentChildren, DequeViewOff) ->
                    List.length model.tableOfContents

                (_, DequeViewOn) ->
                   BoundedDeque.length model.deque

        n =
            n_
                |> String.fromInt

        w =
            140
    in
    case model.currentUser of
        Nothing ->
            case model.documentListDisplay of
                (SearchResults, _) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.sortByMostRecentFirst model, Button.sortAlphabetical model]
--
--                    Input.button []
--                        { onPress = Just (SetDocumentListType DocumentChildren)
--                        , label =
--                            el (Button.headingStyle w Style.charcoal)
--                                (Element.text ("Public Documents! (" ++ n ++ ")"))
--                        }

                (DocumentChildren, _) ->
                    Input.button []
                        { onPress = Just (SetDocumentListType SearchResults)
                        , label =
                            el (Button.headingStyle w Style.charcoal)
                                (Element.text ("Contents! (" ++ n ++ ")"))
                        }
--
--                (_, DequeViewOn) ->
--                    Input.button []
--                        { onPress = Just ToggleDequeview
--                        , label =
--                            el (Button.headingStyle w Style.charcoal)
--                                (Element.text "Recent")
--                      }

        Just _ ->
            case model.documentListDisplay of
                (SearchResults, DequeViewOff) ->

                    row [ spacing 10 ] [ Button.setDocumentListType model w n, Button.sortByMostRecentFirst model, Button.sortAlphabetical model, Button.setDequeView model]

                (SearchResults, DequeViewOn) ->
                    row [ spacing 10 ] [ Button.setDocumentListType model w n,  Button.setDequeView model]

                (DocumentChildren, DequeViewOff) ->
                    row [ spacing 10 ] [Button.setDocumentChildren model w n, Button.setDequeViewX model w n]

                (DocumentChildren, DequeViewOn) ->
                    row [ spacing 10 ] [Button.setDocumentListType model w n, Button.setDequeViewX model w n]



-- HEADERS


readingHeader : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
readingHeader viewInfo model rt =
    let
        lhWidth =
            scale (viewInfo.toolStripWidth + viewInfo.docListWidth) model.windowWidth

        -- scale viewInfo.docListWidth model.windowWidth
        titleWidth =
            scale (0.75 * viewInfo.renderedDisplayWidth) model.windowWidth

        rhWidth =
            scale (0.25 * viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ modeButtonStrip model lhWidth
        , row [ spacing 10, alignLeft ]
            [ titleRow titleWidth rt
            , searchRow model
            ]
        ]


simpleEditingHeader : ViewInfo -> Model -> Element Msg
simpleEditingHeader viewInfo model =
    let
        lhWidth =
            scale (viewInfo.toolStripWidth + viewInfo.docListWidth + viewInfo.editorWidth / 2) model.windowWidth

        rh =
            viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth

        --        titleWidth =
        --            scale (rh / 2) model.windowWidth
        titleWidth =
            scale (0.45 * rh) model.windowWidth

        rhWidth =
            scale (viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ modeButtonStrip model lhWidth
        , row [ spacing 10, width fill ]
            [ el [ Font.color Style.white ] (Element.text "Subdocument Editor")
            , searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]


editingHeader : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
editingHeader viewInfo model rt =
    let
        lhWidth =
            scale (viewInfo.toolStripWidth + viewInfo.docListWidth + viewInfo.editorWidth / 2) model.windowWidth

        rh =
            viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth

        --        titleWidth =
        --            scale (rh / 2) model.windowWidth
        titleWidth =
            scale (0.45 * rh) model.windowWidth

        rhWidth =
            scale (viewInfo.editorWidth / 2 + viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ modeButtonStrip model lhWidth
        , row [ spacing 10, width fill ]
            [ -- titleRowForEditing titleWidth rt
             searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]


searchRow model =
    row [ spacing 10, alignRight ] [ Search.inputTerms model, Button.clearSearchTerms,  Button.search model,  Button.allDocuments,  Button.helpDocs ]


titleRowForEditing titleWidth rt =
    row [  height (px 40), width (px titleWidth), Font.color Style.white, alignRight, clipX ]
        [ row [ width (px 200), alignRight, clipX , clipY, height (px 40), Font.size 8] [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ] ]


titleRow titleWidth rt =
    row [ Font.size 24, height (px 40), width (px titleWidth), Font.color Style.white, alignRight, clipX ]
        [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ]


editTools : Model -> Element Msg
editTools model =
    case model.currentUser of
        Nothing -> Element.none
        Just _ ->
            if List.member model.appMode [ Editing StandardEditing, Editing SubdocumentEditing ] then
                row [ spacing 6, width (px 360) ]
                    [ Button.editingMode model
                    , Button.subDocumentEditingMode model
                    , Button.newDocument model
                    , Button.firstSubDocument model
                    , Button.saveDocument model
                    , Button.deleteDocument model
                    , Button.cancelDeleteDocumentInHeader model
                    ]

            else
                row [ spacing 6 ] [ Button.editingMode model, Button.subDocumentEditingMode model ]



-- TAB-STRIP ON LEFT --


tabStrip : ViewInfo -> Model -> Element Msg
tabStrip viewInfo model =
    column [ width (px 30), height (px 200), Background.color (Style.grey 0.1), alignTop ]
        [ row [ spacing 15, rotate -1.5708, moveLeft 50, moveDown 70 ] [ Button.showTools model, Button.showDocumentList model ]
        ]



-- FOOTER --


footer : Model -> Element Msg
footer model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ currentAuthorDisplay model
        -- , el [] (Element.text <| slugOfCurrentDocument model)
        , Button.getTextSelection
        , dirtyDocumentDisplay model
        , wordCount model
        , row [ spacing 4 ] [ Button.totalWordCount, totalWordCountDisplay model ]
        , Utility.View.showIf (Maybe.map .username model.currentUser ==  Just "jxxcarlson") Button.downloadArchive
        , Utility.View.showIf (Maybe.map .username model.currentUser ==  Just "jxxcarlson") Button.uploadArchive
         ,Utility.View.showIf (Maybe.map .username model.currentUser ==  Just "jxxcarlson") Button.saveImportedArchive
        , Button.shareUrl model
        , shareUrlDisplay model

        , displayMessage model.message
        -- , currentTime model
        ]



shareUrlDisplay : Model -> Element Msg
shareUrlDisplay model =
    case (model.currentDocument, model.flashCounterForShareUrl > 0) of
        (Nothing, _) -> Element.none
        (_, False) -> Element.none
        (Just doc, True) ->
            case doc.public of
                True ->
                   -- el [] (Element.text <| Config.endpoint ++ "/#id/" ++ Uuid.toString doc.id)
                   el [] (Element.text <| Config.endpoint ++ "/#doc/" ++ doc.slug)

                False ->
                    el [] (Element.text "Document is private, can't share")

displayMessage : Message -> Element Msg
displayMessage (messageType, str) =
    case messageType of
        ErrorMessage -> el [Font.color Style.white, Background.color (Style.brightRed),  alignRight, Font.size 12, Font.bold,  paddingXY 10 4, centerY] (Element.text str)
        _ -> el [alignRight, paddingXY 10 0] (Element.text str)



totalWordCountDisplay model =
    if model.flashCounterForTotalWordCount == 0 then
        Element.none

    else
        let
            words =
                model.totalWordCount

            pages =
                Basics.round <| toFloat words / Config.wordsPerPage

            t =
                String.fromInt words ++ " (" ++ String.fromInt pages ++ " pages)"
        in
        el [] (Element.text t)


showToken : Model -> Element Msg
showToken model =
    case model.token of
        Nothing ->
            el [] (Element.text "Token: --")

        Just token ->
            el [] (Element.text <| "Token: " ++ token)


displayLevels model =
    case model.currentDocument of
        Nothing ->
            Element.none

        Just doc ->
            let
                levels =
                    doc.childInfo
                        |> List.map (Tuple.second >> String.fromInt)
                        |> String.join ", "
            in
            el [] (Element.text <| "Levels: " ++ levels)


displayToggle model =
    if model.toggleToc then
        el [] (Element.text "Toggle ON")

    else
        el [] (Element.text "Toggle OFF")


tocCursorDisplay : Model -> Element Msg
tocCursorDisplay model =
    case model.tocCursor of
        Nothing ->
            el [] (Element.text "No focus")

        Just uuid ->
            el [] (Element.text <| "Focus: " ++ Uuid.toString uuid)


dirtyDocumentDisplay : Model -> Element Msg
dirtyDocumentDisplay model =
    case ( model.appMode == Editing StandardEditing, model.currentUser ) of
        ( True, Just _ ) ->
            dirtyDocumentDisplay_ model

        ( _, _ ) ->
            Element.none


dirtyDocumentDisplay_ model =
    if model.currentDocumentDirty then
        row [ width (px 30), height (px 20), Background.color Style.red, Font.color Style.white ]
            [ el [ centerX, centerY ] (Element.text <| String.fromInt model.secondsWhileDirty) ]

    else
        row [ width (px 30), height (px 20), Background.color Style.green, Font.color Style.white ]
            [ el [ centerX, centerY ] (Element.text <| String.fromInt model.secondsWhileDirty) ]


slugOfCurrentDocument : Model -> String
slugOfCurrentDocument model =
    case model.currentDocument of
        Nothing ->
            ""

        Just document ->
            document.slug


currentAuthorDisplay model =
    let
        message =
            case model.currentUser of
                Nothing ->
                    ( UserMessage, "Not signed in" )

                Just user ->
                    ( UserMessage, "Signed in as " ++ user.username )
    in
    Element.el [ Font.color Style.white, Font.size 12 ]
        (Element.text <| messageContent <| message)


messageContent : Message -> String
messageContent ( _, messageContent_ ) =
    messageContent_


currentTime model =
    Element.el [ alignRight, Font.color Style.white, Font.size 12 ]
        (Element.text <| "Current time: " ++ Utility.Time.humanTimeHM model.zone model.time)


wordCount model =
    let
        sourceText =
            case model.currentDocument of
                Just document ->
                    document.content

                _ ->
                    ""
        (wc, pc) = Utility.pageAndWordCount sourceText
          |> (\(x,y) -> (String.fromInt x, String.fromInt y))

        legend = interpolate "Word count {0} ({1} pages)" [wc, pc]
    in
    Element.el [ Font.color Style.white, Font.size 12 ] (Element.text <| legend )


status model =
    el [ Font.color Style.white, Font.size 12, centerX ]
        (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)


docTypePanel model =
    let
        w =
            120
    in
    column [ spacing 0 ]
        [ el [ Font.color Style.white, Font.bold, paddingXY 0 8 ] (Element.text "Document Type")
        , Button.miniLaTeX model w
        , Button.extendedMarkdown model w
        , Button.extendedMathMarkdown model w
        ]




