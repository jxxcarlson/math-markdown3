module Main exposing (main, parseSearchTerm)

import Api.InputObject exposing (Document_order_by(..))
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import CustomElement.CodeEditor as Editor
import Data
import Browser.Dom as Dom
import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Element exposing (..)
import Element.Background as Background
import UrlAppParser exposing(Route(..))
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import BoundedDeque exposing(BoundedDeque)
import Element.Lazy
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (..)
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Keyboard exposing (Key(..))
import List.Extra
import Outside
import Markdown.Elm
import Markdown.ElmWithId
import Markdown.Option exposing (Option(..))
import ParseWithId
import Preprocessor
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Random
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData(..))
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..), orderByMostRecentFirst, orderByTitleAsc)
import Style
import Task exposing (Task)
import Time exposing (Posix)
import Toc exposing (TocItem)
import TocManager
import TocZ exposing (TocMsg(..), viewZ)
import Tree exposing (Tree)
import Tree.Diff as Diff
import Tree.Zipper as Zipper exposing (Zipper)
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


      VIEW UTILITIES: showIf, etc.


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

--application :
--  { init : flags -> Url -> Key -> ( model, Cmd msg )
--  , view : model -> Document msg
--  , update : msg -> model -> ( model, Cmd msg )
--  , subscriptions : model -> Sub msg
--  , onUrlRequest : UrlRequest -> msg
--  , onUrlChange : Url -> msg
--  }

-- MODEL --


type alias RenderedText msg =
    { title : Html msg, toc : Html msg, document : Html msg }


type alias Model  =
    { seed : Int
    -- NAV
      , key : Nav.Key
      , url : Url.Url

    -- UI
    , docType : Document.DocType
    , windowWidth : Int
    , windowHeight : Int
    , visibilityOfTools : Visibility
    , appMode : AppMode
    , documentListType : DocumentListType
    , message : Message
    , pressedKeys : List Key
    , focusedElement : FocusedElement
    , flashCount : Int

    -- SYSTEM
    , currentSeed : Seed
    , currentUuid : Uuid.Uuid
    , zone : Time.Zone
    , time : Time.Posix

    -- USER
    , currentUser : Maybe User
    , token : Maybe String
    , username : String
    , email : String
    , password : String
    , passwordConfirmation : String
    , newPassword1 : String
    , newPassword2 : String

    -- EDITOR
    , selectedText : String

    -- DOCUMENT
    , counter : Int
    , documentDeleteState : DocumentDeleteState
    , documentList : List Document
    , tableOfContents : List Document
    , deque : BoundedDeque Document
    , totalWordCount : Int
    , tocData : Maybe (Zipper TocItem)
    , tocCursor : Maybe Uuid
    , toggleToc : Bool
    , candidateChildDocumentList : List Document
    , childDocIdString : String
    , currentDocument : Maybe Document
    , currentDocumentDirty : Bool
    , secondsWhileDirty : Int
    , lastAst : Tree ParseWithId.MDBlockWithId
    , renderedText : RenderedText Msg
    , tagString : String
    , searchTerms : String
    , sortTerm : OptionalArgument (List Document_order_by)
    , searchMode : SearchMode
    , sortMode : SortMode
    , documentOutline : String
    }



-- TYPES FOR MODEL

type UrlRequest
  = Internal Url.Url
  | External String

type DocumentListType
    = SearchResults
    | DequeView
    | DocumentChildren


type SearchMode
    = UserSearch
    | PublicSearch


type SortMode
    = MostRecentFirst
    | Alphabetical


type alias Message =
    ( MessageType, String )


type MessageType
    = SignInMessage
    | UserMessage
    | AuthMessage
    | ErrorMessage
    | DebugMessage


type FocusedElement
    = FocusOnSearchBox
    | NoFocus


type AppMode
    = Reading
    | Editing EditMode
    | UserMode UserState


type EditMode
    = StandardEditing
    | SubdocumentEditing


type UserState
    = SignInState
    | SignUpState
    | ChangePasswordState
    | SignedInState


type DocumentDeleteState
    = SafetyOn
    | Armed


type Visibility
    = Visible
    | Invisible


type SearchType
    = TitleSearch
    | KeywordSearch
    | NoSearchTerm


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

        model : Model
        model =
            { seed = 0
            , key = key
            , url = url


            -- UI
            , docType = Markdown MDExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , visibilityOfTools = Invisible
            , appMode = UserMode SignInState
            , documentListType = SearchResults
            , message = ( UserMessage, "Starting ..." )
            , pressedKeys = []
            , focusedElement = NoFocus
            , flashCount = 0

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
             , selectedText = ""
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
            , currentDocument = Nothing
            , currentDocumentDirty = False
            , secondsWhileDirty = 0
            , lastAst = emptyAst
            , renderedText = emptyRenderedText
            , tagString = ""
            , searchTerms = ""
            , sortTerm = orderByMostRecentFirst
            , searchMode = PublicSearch
            , sortMode = MostRecentFirst
            , documentOutline = ""
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Request.publicDocuments hasuraToken |> Cmd.map Req
        , resetViewportOfRenderedText
        , resetViewportOfEditor
        , Outside.sendInfo (Outside.AskToReconnectUser E.null)
        , Outside.sendInfo (Outside.AskForDequeData E.null)
        , processUrl flags.location

        ]
    )



-- MSG --


type Msg
    = NoOp
      -- System
    | NewUuid
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    -- Ports
    | Outside Outside.InfoForElm
    | LogErr String
      -- Random
    | GenerateSeed
    | NewSeed Int
    -- Navigation
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ScrollAttempted (Result Dom.Error ())
      -- UI
    | SetToolPanelState Visibility
    | SetAppMode AppMode
    | WindowSize Int Int
    | KeyMsg Keyboard.Msg
    | SetFocusOnSearchBox (Result Dom.Error ())
      -- User
    | GotUserName String
    | GotPassword String
    | GotPasswordConfirmation String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | SignUp
    | SignOut
      -- Editor
    | ProcessLine String
    | SetViewPortForElement (Result Dom.Error ( Dom.Element, Dom.Viewport ))
    | GetTextSelection
      -- Document
    | CreateDocument
    | SaveDocument
    | GetUserDocuments
    | AllDocuments
    | GetPublicDocuments
    | GetHelpDocs
    | SetSortMode SortMode
    | SetDocumentListType DocumentListType
    | SetDocType DocType
    | SetCurrentDocument Document
    | SetCurrentSubDocument Document TocItem
      -- Subdocuments
    | NewSubdocument
    | FirstSubdocument
    | AddSubdocument
    | DeleteSubdocument
    | SetUpOutline
    | UpdateChildren
    | GotOutline String
    | AddThisDocumentToMaster Document
    | GotChildDocIdString String
    | DoTotalWordCount
      -- Doc Search
    | ClearSearchTerms
    | GotSearchTerms String
    | DoSearch
    | ToggleSearchMode
      -- Doc Delete
    | ArmForDelete
    | DeleteDocument
    | CancelDeleteDocument
      -- Doc Update
    | UpdateDocumentText String
    | SetDocumentPublic Bool
    | GotSecondPart (RenderedText Msg)
    | GotTagString String
    | Clear
    | Req RequestMsg
    | TOC TocMsg



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    -- XYXY
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize WindowSize
        , Sub.map KeyMsg Keyboard.subscriptions
        , Outside.getInfo Outside LogErr
        ]



-- UPDATE FUNCTION


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                                  , documentListType = SearchResults
                                  , focusedElement = NoFocus
                                  , visibilityOfTools = Invisible
                                , token =  Just outsideUser.token}
                            , getUserDocumentsAtSignIn user)

                Outside.GotSelection selection ->
                    ({model | selectedText = selection, message = (UserMessage, String.left 16 selection)}, Cmd.none)

                Outside.UuidList uuidList ->
                    (model, Request.documentsInIdList hasuraToken uuidList GotDequeDocuments |> Cmd.map Req)

        LogErr err ->
            ({model | message = (ErrorMessage, err)}, Cmd.none)


        Clear ->
            ( { model
                | counter = model.counter + 1
              }
            , Cmd.none
            )

        SetDocumentListType documentListType ->
            case documentListType of
                SearchResults ->
                    ( { model | documentListType = SearchResults }, Cmd.none )

                DocumentChildren ->
                    ( { model | documentListType = DocumentChildren }, Cmd.none )

                DequeView ->
                    ( { model | documentListType = DequeView }, Cmd.none )


        SetSortMode sortMode ->
            let
                sortTerm =
                    case sortMode of
                        Alphabetical ->
                            orderByTitleAsc

                        MostRecentFirst ->
                            orderByMostRecentFirst

                cmd =
                    case model.currentUser of
                        Just user ->
                            Tuple.second <| doSearch model

                        Nothing ->
                            Cmd.none
            in
            ( { model | sortMode = sortMode, sortTerm = sortTerm }, cmd )

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
                    setModeToReading model

                Editing editMode ->
                    setModeToEditing model editMode

                UserMode s ->
                    setUserMode model s

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
            keyboardGateway model ( pressedKeys, maybeKeyChange )

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
            ( { model
                | currentUser = Nothing
                , token = Nothing
                , appMode = UserMode SignInState
                , username = ""
                , password = ""
                , currentDocument = Nothing
                , documentList = []
              }
            , Request.publicDocuments hasuraToken |> Cmd.map Req
            )


        -- EDITOR --
        ProcessLine str ->
            let
                id =
                    (case Markdown.ElmWithId.searchAST str model.lastAst of
                        Nothing ->
                            "??"

                        Just id_ ->
                            id_ |> ParseWithId.stringOfId)
            in
            ( { model | message = ( UserMessage, "str = " ++ String.left 20 str ++ " -- Clicked on id: " ++ id ) }
            , setViewportForElement id
            )

        SetViewPortForElement result ->
            case result of
                Ok ( element, viewport )  ->
                    ( model, setViewPortForSelectedLine element viewport )

                Err _ ->
                   ({ model | message =  (ErrorMessage, (Tuple.second model.message) ++ ", doc VP ERROR") }, Cmd.none )

        GetTextSelection ->
            (model, Outside.sendInfo (Outside.GetTextSelectionFromOutside E.null))

        -- DOCUMENT --
        CreateDocument ->
            makeNewDocument model

        NewSubdocument ->
            newSubdocument model

        FirstSubdocument ->
            firstSubdocument model

        AddSubdocument ->
            addSubdocument model

        DeleteSubdocument ->
            deleteSubdocument model

        SaveDocument ->
            saveDocument model

        ArmForDelete ->
            ( { model | documentDeleteState = Armed, message = ( UserMessage, "Armed for delete.  Caution!" ) }, Cmd.none )

        CancelDeleteDocument ->
            ( { model | documentDeleteState = SafetyOn, message = ( UserMessage, "Delete cancelled" ) }, Cmd.none )

        DeleteDocument ->
            deleteDocument model

        GetUserDocuments ->
            searchForUsersDocuments model

        GotSecondPart rt ->
            ( { model | renderedText = rt }, Cmd.none )

        AllDocuments ->
            getAllDocuments model

        GetPublicDocuments ->
            searchForPublicDocuments model

        GetHelpDocs ->
            getHelpDocs model

        AddThisDocumentToMaster document ->
            addDocumentToMaster model document

        GotOutline str ->
            ( { model | documentOutline = str }, Cmd.none )

        UpdateDocumentText str ->
            updateDocumentText model (Preprocessor.apply str)

        SetCurrentDocument document ->
            processDocument model document (sendDequeOutside  model)


        SetCurrentSubDocument document tocItem ->
            setCurrentSubdocument model document tocItem

        SetUpOutline ->
            ( setupOutline model, Cmd.none )

        UpdateChildren ->
            case List.head model.tableOfContents of
                Nothing ->
                    ( model, Cmd.none )

                Just masterDocument ->
                    case TocManager.updateMasterAndDocumentListFromOutline model.documentOutline model.tableOfContents of
                        Err error ->
                            ( {model | message = (ErrorMessage, Document.stringOfError   error)}, Cmd.none )

                        Ok ( newMasterDocument, newDocumentList ) ->
                            ( { model
                                | currentDocument = Just newMasterDocument
                                , tableOfContents = newDocumentList
                                , tocData = TocManager.setupWithFocus masterDocument.id (Just newMasterDocument) (List.drop 1 newDocumentList)
                                , tocCursor = Just masterDocument.id
                              }
                            , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
                            )

        SetDocumentPublic bit ->
            setDocumentPublic model bit

        GotTagString str ->
            processTagString model str

        GotChildDocIdString str ->
            ( { model | childDocIdString = str }, Cmd.none )

        DoTotalWordCount ->
            ( { model | totalWordCount = Document.totalWordCount model.tableOfContents, flashCount = config.maxFlashCount }, Cmd.none )

        GotSearchTerms str ->
            ( { model | searchTerms = str, focusedElement = FocusOnSearchBox }, Cmd.none )

        DoSearch ->
            doSearch model

        ToggleSearchMode ->
            toggleSearchMode model

        ClearSearchTerms ->
            clearSearchTerms model

        TOC tocMsg ->
            case tocMsg of
                Focus id ->
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
                                            renderUpdate model document
                            in
                            ( { newModel
                                | currentDocument = currentDocument
                                , tocData = Just (TocZ.focus id zipper)
                                , tocCursor = Just id
                              }
                            , cmd
                            )

                Toggle ->
                    ( { model | toggleToc = not model.toggleToc }, Cmd.batch [resetViewportOfRenderedText, resetViewportOfRenderedText])  -- Cmd.none |> Cmd.map TOC )


        -- NAVIGATION --

        -- XYXY

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
              ( { model | url = url }
              ,  scrollIfNeeded id --   jumpToID id -- ("#" ++ id)
              )


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
                            processDocumentRequest model Nothing documentList

                GotChildDocuments remoteData ->
                    case remoteData of
                        NotAsked ->
                            ( { model | message = ( ErrorMessage, "Get child docs: not asked" ) }, Cmd.none )

                        Loading ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: loading" ) }, Cmd.none )

                        Failure _ ->
                            ( { model | message = ( ErrorMessage, "Get child docs:: request failed" ) }, Cmd.none )

                        Success documentList ->
                            processChildDocumentRequest model documentList

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
                                                 updateMaybeUserWithDeque newDeque model.currentUser
                               cmd = case BoundedDeque.isEmpty  newDeque of
                                   False -> Cmd.none
                                   True ->
                                     let
                                        docIds  = case model.currentUser of
                                                      Nothing -> []
                                                      Just user -> user.recentDocs
                                        _ = "SETTING UO CMD"
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
                            processCandidateChildDocumentRequest model documentList

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
                                    processDocumentRequest model currentDoc documentList
                            in
                            ( { newModel | currentDocument = currentDoc }, cmd )

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
                                 , currentUser = updateMaybeUserWithDeque newDeque model.currentUser
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
                                        , documentListType = SearchResults
                                        , focusedElement = NoFocus
                                        , visibilityOfTools = Invisible
                                      }
                                    ,
                                    Cmd.batch [
                                      getUserDocumentsAtSignIn user
                                      , Cmd.batch[tokenCmd, dequeCommand]
                                      ]
                                      -- XXX
                                    )



-- NAVIGATION HELPERS --

scrollIfNeeded : String -> Cmd Msg
scrollIfNeeded tag =
      Task.attempt ScrollAttempted (
        Dom.getElement tag
          |> Task.andThen (\info -> Dom.setViewportOf "__rt_scroll__" 0  (info.element.y - info.element.height - 40) ))

          -- |> Task.andThen (\info -> Dom.setViewportOf "__rt_scroll__" 0 ((Debug.log "V" info.viewport.y) + (Debug.log "Y" info.element.y))))

pushDocument : Document -> Cmd Msg
pushDocument document =
    Outside.pushUrl <| "/" ++ Uuid.toString  document.id


processUrl : String -> Cmd Msg
processUrl urlString = Cmd.none
--    case UrlAppParser.toRoute urlString of
--        NotFound ->
--            Cmd.batch
--                [
----                  Outside.sendInfoOutside (AskToReconnectDocument Encode.null)
----                , Outside.sendInfoOutside (AskToReconnectDocumentList Encode.null)
----                , Outside.sendInfoOutside (AskToReconnectRecentDocumentQueue Encode.null)
----                , Outside.sendInfoOutside (AskToReconnectUser Encode.null)
--
--                ]
--
--        DocumentRef docId ->
--            Cmd.batch
--                [
----                  Outside.sendInfoOutside (AskToReconnectUser Encode.null)
----                , Outside.sendInfoOutside (AskToReconnectRecentDocumentQueue Encode.null)
----
----                --, Outside.sendInfoOutside (AskToReconnectDocumentList Encode.null)
----                , Cmd.map DocMsg (Document.getDocumentById docId Nothing)
----                , Cmd.map DocListMsg (DocumentList.findDocuments Nothing <| "id=" ++ String.fromInt docId)
--                ]
--
--        HomeRef username ->
--            Cmd.batch
--                [
----                 Outside.sendInfoOutside (AskToReconnectUser Encode.null)
----                , Outside.sendInfoOutside (AskToReconnectRecentDocumentQueue Encode.null)
----                , Cmd.map DocListMsg (DocumentList.findDocuments Nothing ("key=home&authorname=" ++ username))
--                ]
--
--        InternalRef str ->
--            Cmd.none

-- KEYBOARD HELPERS -


keyboardGateway : Model -> ( List Key, Maybe Keyboard.KeyChange ) -> ( Model, Cmd Msg )
keyboardGateway model ( pressedKeys, maybeKeyChange ) =
    if List.member Control model.pressedKeys then
        handleKey { model | pressedKeys = [] } (headKey pressedKeys)

    else if model.focusedElement == FocusOnSearchBox && List.member Enter model.pressedKeys then
        let
            newModel =
                { model | pressedKeys = [] }
        in
        case model.appMode == Editing SubdocumentEditing of
            True ->
                searchForChildDocuments model

            False ->
                doSearch newModel

    else
        ( { model | pressedKeys = pressedKeys }, Cmd.none )


handleKey : Model -> Key -> ( Model, Cmd Msg )
handleKey model key =
    case key of
        Character "a" ->
            getAllDocuments model

        Character "e" ->
            setModeToEditing model StandardEditing

        --        Character "f" ->
        --            ( model, focusSearchBox )
        Character "h" ->
            getHelpDocs model

        Character "f" ->
            clearSearchTerms model

        Character "n" ->
            makeNewDocument model

        Character "r" ->
            setModeToReading model

        Character "s" ->
            saveDocument model

        Character "t" ->
            toggleKeyboardTools model

        Character "u" ->
            case model.currentUser of
                Nothing ->
                    setUserMode model SignInState

                _ ->
                    setUserMode model SignedInState

        _ ->
            ( model, Cmd.none )


headKey : List Key -> Key
headKey keyList =
    keyList
        |> List.filter (\item -> item /= Control && item /= Character "^")
        |> List.head
        |> Maybe.withDefault F20



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
        flashCount =
            if model.flashCount > 0 then
                model.flashCount - 1

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
                        if user.username == document.authorIdentifier then
                            Request.updateDocument hasuraToken document |> Cmd.map Req

                        else
                            Cmd.none

            else
                Cmd.none
    in
    ( { model | time = newTime, secondsWhileDirty = secondsWhileDirty, flashCount = flashCount }
    , cmd
    )


-- OUTSIDE HELPERS --

sendDequeOutside : Model -> Cmd Msg
sendDequeOutside  model =
    let
       data : E.Value
       data =
           model.deque
             |> Document.idListOfDeque
             |> Document.encodeStringList "deque"
    in
      Outside.sendInfo (Outside.DequeData data) |> Cmd.map Req


-- EDITOR HELPERS, VIEWPORT

masterId = "_rendered_text_"

resetViewportOfRenderedText : Cmd Msg
resetViewportOfRenderedText =
  Task.attempt (\_ -> NoOp) (Dom.setViewportOf masterId -100 0)

resetViewportOfEditor : Cmd Msg
resetViewportOfEditor =
  Task.attempt (\_ -> NoOp) (Dom.setViewportOf "_editor_" 0 0)



setViewportForElement : String -> Cmd Msg
setViewportForElement id =
    Dom.getViewportOf  masterId
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
            viewport.viewport.y + element.element.y - element.element.height - 150
    in
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf masterId 0 y)



-- USER HELPERS


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

getUserDocumentsAtSignIn : User -> Cmd Msg
getUserDocumentsAtSignIn user =
    Request.documentsWithAuthorAndTitleSorted hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

-- SEARCH HELPERS


stringValueOfSearchType : String -> SearchType
stringValueOfSearchType str =
    case str of
        "k" ->
            KeywordSearch

        _ ->
            TitleSearch


parseSearchTerm : String -> ( SearchType, String )
parseSearchTerm str =
    let
        parts =
            String.split "/" str

        first =
            List.head parts

        second =
            List.head (List.drop 1 parts)
    in
    case ( first, second ) of
        ( Just searchTerm, Just typeString ) ->
            ( stringValueOfSearchType typeString, searchTerm )

        ( Just searchTerm, Nothing ) ->
            ( TitleSearch, searchTerm )

        ( _, _ ) ->
            ( NoSearchTerm, "" )


toggleSearchMode : Model -> ( Model, Cmd Msg )
toggleSearchMode model =
    case model.searchMode of
        UserSearch ->
            ( { model | searchMode = PublicSearch }, Cmd.none )

        PublicSearch ->
            case model.currentUser of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    ( { model | searchMode = UserSearch }, Cmd.none )


clearSearchTerms model =
    ( { model | searchTerms = "" }, focusSearchBox )


inputSearchTerms model =
    Input.text (Style.inputStyle 200 ++ [ setElementId "search-box" ])
        { onChange = GotSearchTerms
        , text = model.searchTerms
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, width (px 0) ] (Element.text "")
        }


searchButton : Model -> Element Msg
searchButton model =
    let
        title =
            case model.searchMode of
                UserSearch ->
                    "My docs"

                PublicSearch ->
                    "Public docs"
    in
    Input.button []
        { onPress = Just ToggleSearchMode
        , label =
            el [ height (px 30), width (px 75), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text title))
        }


sortAlphabeticalButton : Model -> Element Msg
sortAlphabeticalButton model =
    let
        color =
            case model.sortMode == Alphabetical of
                True ->
                    Style.darkRed

                False ->
                    Style.charcoal
    in
    Input.button (Style.standardButton ++ [ Background.color color, Font.color Style.white ])
        { onPress = Just (SetSortMode Alphabetical)
        , label = el [] (Element.text "A")
        }


sortByMostRecentFirstButton : Model -> Element Msg
sortByMostRecentFirstButton model =
    let
        color =
            case model.sortMode == MostRecentFirst of
                True ->
                    Style.darkRed

                False ->
                    Style.charcoal
    in
    Input.button (Style.standardButton ++ [ Background.color color, Font.color Style.white ])
        { onPress = Just (SetSortMode MostRecentFirst)
        , label = el [] (Element.text "R")
        }


allDocumentsButton : Element Msg
allDocumentsButton =
    Input.button []
        { onPress = Just AllDocuments
        , label =
            el [ height (px 30), width (px 40), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text "All"))
        }


helpDocsButton : Element Msg
helpDocsButton =
    Input.button []
        { onPress = Just GetHelpDocs
        , label =
            el [ height (px 30), width (px 40), padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2, centerX ] (Element.text "Help"))
        }


clearSearchTermsButton : Element Msg
clearSearchTermsButton =
    Input.button []
        { onPress = Just ClearSearchTerms
        , label =
            el [ height (px 30), width (px 25), centerX, padding 8, Background.color Style.blue, Font.color Style.white, Font.size 11 ]
                (el [ moveDown 2 ] (Element.text "X"))
        }


focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt SetFocusOnSearchBox (Dom.focus "search-box")


getAllDocuments : Model -> ( Model, Cmd Msg )
getAllDocuments model =
    let
        cmd =
            case model.searchMode of
                UserSearch ->
                    case model.currentUser of
                        Nothing ->
                            Cmd.none

                        Just user ->
                            Request.documentsWithAuthorAndTitleSorted hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

                PublicSearch ->
                    Request.publicDocumentsWithTitle hasuraToken "" |> Cmd.map Req
    in
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


getHelpDocs : Model -> ( Model, Cmd Msg )
getHelpDocs model =
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }
    , Request.publicDocumentsWithTag hasuraToken "usermanual" |> Cmd.map Req
    )


doSearch : Model -> ( Model, Cmd Msg )
doSearch model =
    case model.searchMode of
        UserSearch ->
            searchForUsersDocuments model

        PublicSearch ->
            searchForPublicDocuments model


searchForUsersDocuments : Model -> ( Model, Cmd Msg )
searchForUsersDocuments model =
    let
        authorIdentifier =
            model.currentUser |> Maybe.map .username |> Maybe.withDefault "__nobodyHere__"

        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTitleSorted hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


searchForChildDocuments : Model -> ( Model, Cmd Msg )
searchForChildDocuments model =
    let
        authorIdentifier =
            model.currentUser |> Maybe.map .username |> Maybe.withDefault "__nobodyHere__"

        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTitleSorted hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( model, cmd )


searchForPublicDocuments : Model -> ( Model, Cmd Msg )
searchForPublicDocuments model =
    let
        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.publicDocumentsWithTitle hasuraToken searchTerm |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.publicDocumentsWithTag hasuraToken searchTerm |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )



-- DOCUMENT HELPERS --


emptyAst : Tree ParseWithId.MDBlockWithId
emptyAst =
    Markdown.ElmWithId.parse -1 ExtendedMath ""


emptyRenderedText : RenderedText Msg
emptyRenderedText =
    render (Markdown MDExtendedMath) emptyAst


renderAstFor : Tree ParseWithId.MDBlockWithId -> Cmd Msg
renderAstFor ast =
    Process.sleep 10
        |> Task.andThen
            (\_ ->
                Process.sleep 100
                    |> Task.andThen (\_ -> Task.succeed (Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" ast))
            )
        |> Task.perform GotSecondPart



--renderSecond : Model -> Cmd Msg
--renderSecond model =
--    renderAstFor model model.sourceText


getFirstPart : String -> String
getFirstPart str =
    String.left 2000 str


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
            case currentDoc of
                Nothing ->
                    ( emptyAst, emptyRenderedText, Cmd.none )

                Just doc ->
                    let
                        content =
                            doc.content

                        lastAst =
                            parse doc.docType model.counter content

--                        nMath =
--                            Markdown.ElmWithId.numberOfMathElements lastAst

                        ( renderedText, cmd_ ) =
--                            -- XYXY
--                            if nMath > 1000 then
--                                let
--                                    firstAst =
--                                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart content)
--
--                                    renderedText_ =
--                                        render doc.docType firstAst
--
--                                    cmd__ =
--                                        renderAstFor lastAst
--                                in
--                                ( renderedText_
--                                , cmd__
--                                )
--
--                            else
                                ( render doc.docType lastAst, Cmd.none )
                    in
                    ( lastAst, renderedText, cmd_ )
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


processChildDocumentRequest : Model -> List Document -> ( Model, Cmd Msg )
processChildDocumentRequest model documentList =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just masterDocument ->
            let
                sortedChildDocuments =
                    Document.sortChildren masterDocument documentList

                newDocumentList =
                    masterDocument :: sortedChildDocuments
            in
            ( { model
                | tableOfContents = newDocumentList
                , tocData = TocManager.setup (Just masterDocument) documentList
                , tocCursor = Just masterDocument.id
                , message = ( UserMessage, "Child documents: " ++ String.fromInt (List.length documentList) )
              }
            , Cmd.none
            )


processCandidateChildDocumentRequest : Model -> List Document -> ( Model, Cmd Msg )
processCandidateChildDocumentRequest model documentList =
    ( { model
        | candidateChildDocumentList = documentList
        , message = ( UserMessage, "Candidate child documents: " ++ String.fromInt (List.length documentList) )
      }
    , Cmd.none
    )


processDocument : Model -> Document -> (Cmd Msg) -> ( Model, Cmd Msg )
processDocument model document extraCmd =
    let
        newDeque = Document.pushFrontUnique document model.deque

        updateDequeCmd = case model.currentUser of
            Nothing -> Cmd.none
            Just user -> Request.updateUser hasuraToken (updateUserWithDeque newDeque user) |> Cmd.map Req

        ( ( newAst, newRenderedText ), cmd, documentListType ) =
            let
                lastAst =
                    Markdown.ElmWithId.parse model.counter ExtendedMath document.content

                nMath =
                    Markdown.ElmWithId.numberOfMathElements lastAst

                renderedText =
                    if nMath > 10 then
                        let
                            firstAst =
                                Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart document.content)
                        in
                        Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" <| firstAst

                    else
                        Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst

                cmd1 =
                    if nMath > 10 then
                        renderAstFor lastAst

                    else
                        Cmd.none

                ( cmd2, documentListType_ ) =
                    if document.childInfo == [] then
                        ( Cmd.none, model.documentListType )

                    else
                        ( Request.documentsInIdList hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req, DocumentChildren )
            in
            ( ( lastAst, renderedText ), Cmd.batch [ cmd1, cmd2, extraCmd, updateDequeCmd ], documentListType_ )

    in
    ( { model
        | currentDocument = Just document
        , documentListType = documentListType
        , counter = model.counter + 2
        , deque = newDeque
        , currentUser = updateMaybeUserWithDeque newDeque model.currentUser
        , lastAst = newAst
        , renderedText = newRenderedText
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
    , Cmd.batch [cmd, resetViewportOfRenderedText, resetViewportOfEditor]
    )


updateMaybeUserWithDeque : BoundedDeque Document -> Maybe User -> Maybe User
updateMaybeUserWithDeque deque maybeUser =
    Maybe.map (\user -> { user | recentDocs = BoundedDeque.toList deque |> List.map .id} ) maybeUser

updateUserWithDeque : BoundedDeque Document -> User -> User
updateUserWithDeque deque user =
    { user | recentDocs = BoundedDeque.toList deque |> List.map .id}


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
        ( ( newAst, newRenderedText ), cmd, documentListType ) =
            let
                lastAst =
                    Markdown.ElmWithId.parse model.counter ExtendedMath document.content

                nMath =
                    Markdown.ElmWithId.numberOfMathElements lastAst

                renderedText =
                    if nMath > 10 then
                        let
                            firstAst =
                                Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart document.content)
                        in
                        Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" <| firstAst

                    else
                        Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst

                cmd1 =
                    if nMath > 10 then
                        renderAstFor lastAst

                    else
                        Cmd.none

                ( cmd2, documentListType_ ) =
                    if document.childInfo == [] then
                        ( Cmd.none, model.documentListType )

                    else
                        ( Request.documentsInIdList hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req, DocumentChildren )
            in
            ( ( lastAst, renderedText ), Cmd.batch [ cmd1, cmd2, resetViewportOfRenderedText, resetViewportOfEditor ], documentListType_ )
    in
    ( { model
        | currentDocument = Just document
        , documentListType = documentListType
        , counter = model.counter + 2
        , lastAst = newAst
        , renderedText = newRenderedText
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
    , cmd
    )


renderUpdate : Model -> Document -> ( Model, Cmd Msg )
renderUpdate model document =
    let
        lastAst =
            Markdown.ElmWithId.parse model.counter ExtendedMath document.content

        nMath =
            Markdown.ElmWithId.numberOfMathElements lastAst

        renderedText =
            if nMath > 10 then
                let
                    firstAst =
                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart document.content)
                in
                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" <| firstAst

            else
                Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst

        cmd1 =
            if nMath > 10 then
                renderAstFor lastAst

            else
                Cmd.none

        cmd2 =
            if document.childInfo == [] then
                Cmd.none

            else
                Request.documentsInIdList hasuraToken (Document.idList document) GotChildDocuments |> Cmd.map Req
    in
    ( { model
        | lastAst = lastAst
        , renderedText = renderedText
        , counter = model.counter + 2
      }
    , Cmd.batch [ cmd1, cmd2 ]
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
                    ( { model | message = ( UserMessage, "Deleting document ..." ), documentDeleteState = SafetyOn }
                    , Request.deleteDocument hasuraToken document |> Cmd.map Req
                    )


makeNewDocument : Model -> ( Model, Cmd Msg )
makeNewDocument model =
    case model.currentUser of
        Nothing ->
            ( model, Cmd.none )

        Just user ->
            let
                newDocumentText =
                    "# New Document\n\nWrite something here ..."

                newDocument =
                    Document.create model.currentUuid user.username "New Document" newDocumentText

                lastAst =
                    Markdown.ElmWithId.parse -1 ExtendedMath newDocumentText

                ( newUuid, newSeed ) =
                    step Uuid.generator model.currentSeed
            in
            ( { model
                | currentDocument = Just newDocument
                , documentList = newDocument :: model.documentList
                , visibilityOfTools = Visible
                , appMode = Editing StandardEditing
                , tagString = ""
                , currentUuid = newUuid
                , currentSeed = newSeed
                , lastAst = lastAst
                , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst
              }
            , Request.insertDocument hasuraToken newDocument |> Cmd.map Req
            )


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
    in
    ( { model
        | currentDocument = Just currentDocument
        , tableOfContents = tableOfContents
        , documentList = newDocumentList
        , documentOutline = newDocumentOutline
        , tocData = TocManager.setupWithFocus masterDocument.id (Just masterDocument) (List.drop 1 tableOfContents)
        , tocCursor = Just currentDocument.id
      }
    , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
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
        [ Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList hasuraToken (newChildInfo |> List.map Tuple.first) GotChildDocuments |> Cmd.map Req
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
    ( { model | currentDocument = Just newMasterDocument }
    , Cmd.batch
        [ Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList hasuraToken (newChildInfo |> List.map Tuple.first)  GotChildDocuments |> Cmd.map Req
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
            "# New subdocument:\n\n### " ++ document.title ++ "\n\nWrite something here ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        lastAst =
            Markdown.ElmWithId.parse -1 ExtendedMath newDocumentText

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
        , documentListType = DocumentChildren
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , lastAst = lastAst
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst
      }
    , Cmd.batch
        [ Request.insertDocument hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument hasuraToken masterDocument |> Cmd.map Req
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
            "# New subdocument:\n\n### " ++ masterDocument.title ++ "\n\nWrite something here ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        lastAst =
            Markdown.ElmWithId.parse -1 ExtendedMath newDocumentText

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
        , documentListType = DocumentChildren
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , lastAst = lastAst
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst
      }
    , Cmd.batch
        [ Request.insertDocument hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        ]
    )


newSubdocumentWithChildren : Model -> User -> Document -> Document -> ( Model, Cmd Msg )
newSubdocumentWithChildren model user masterDocument targetDocument =
    let
        -- Prepare the new document
        newDocumentText =
            "# New subdocument:\n\n### " ++ masterDocument.title ++ "\n\nWrite something here ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        -- Prepare AST and udpate uuid
        lastAst =
            Markdown.ElmWithId.parse -1 ExtendedMath newDocumentText

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
        , counter = model.counter + 1 -- Necessary?
        , documentList = newDocumentList
        , tocData = TocManager.setupWithFocus newDocument.id (Just newMasterDocument) newChildDocumentList
        , tocCursor = Just newDocument.id
        , documentListType = DocumentChildren
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing StandardEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , lastAst = lastAst
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC "Topics" lastAst
      }
    , Cmd.batch
        [ Request.insertDocument hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        ]
    )


updateDocumentText : Model -> String -> ( Model, Cmd Msg )
updateDocumentText model str =
    case model.currentDocument of
        Nothing ->
            ( model, Cmd.none )

        Just doc ->
            let
                updatedDoc1 =
                    Document.setContent str doc

                updatedDoc2 =
                    Document.updateMetaData updatedDoc1

                newAst_ =
                    parse updatedDoc2.docType model.counter str

                newAst =
                    Diff.mergeWith ParseWithId.equal model.lastAst newAst_

                tableOfContents =
                    Document.replaceInList updatedDoc2 model.tableOfContents
            in
            ( { model
                | -- document
                  currentDocument = Just updatedDoc2
                , documentList = Document.replaceInList updatedDoc2 model.documentList
                , tableOfContents = tableOfContents
                , currentDocumentDirty = True
                , tocData = TocManager.setupWithFocus updatedDoc2.id (List.head tableOfContents) (List.drop 1 tableOfContents)
                , tocCursor = Just updatedDoc2.id

                -- rendering
                , lastAst = newAst
                , renderedText = render updatedDoc2.docType newAst
                , counter = model.counter + 1
              }
            , Cmd.none
            )


saveDocument : Model -> ( Model, Cmd Msg )
saveDocument model =
    case ( model.currentUser, model.currentDocument ) of
        ( _, Nothing ) ->
            ( model, Cmd.none )

        ( Nothing, _ ) ->
            ( model, Cmd.none )

        ( Just user, Just document_ ) ->
            if user.username /= document_.authorIdentifier then
                ( model, Cmd.none )

            else
                let
                    document =
                        Document.updateMetaData document_
                in
                ( { model | message = ( UserMessage, "Saving document ..." ), currentDocument = Just document }
                , Request.updateDocument hasuraToken document |> Cmd.map Req
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
                    , documentList = Document.replaceInList newDocument model.documentList
                  }
                , Request.updateDocument hasuraToken newDocument |> Cmd.map Req
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


setModeToReading : Model -> ( Model, Cmd Msg )
setModeToReading model =
    ( { model | appMode = Reading, visibilityOfTools = Invisible }, Cmd.none )


setModeToEditing : Model -> EditMode -> ( Model, Cmd Msg )
setModeToEditing model editMode =
    ( { model
        | appMode = Editing editMode
        , visibilityOfTools = Invisible
        , documentOutline = setupOutline_ model
      }
    , Cmd.none
    )


setUserMode : Model -> UserState -> ( Model, Cmd msg )
setUserMode model s =
    ( { model | appMode = UserMode s }, Cmd.none )


toggleKeyboardTools : Model -> ( Model, Cmd Msg )
toggleKeyboardTools model =
    if model.appMode /= Editing StandardEditing then
        ( model, Cmd.none )

    else
        case model.visibilityOfTools of
            Visible ->
                ( { model | visibilityOfTools = Invisible }, Cmd.none )

            Invisible ->
                ( { model | visibilityOfTools = Visible }, Cmd.none )


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

                lastAst =
                    parse docType model.counter newDocumentText
            in
            ( { model
                | documentList = newDocumentList
                , currentDocument = List.head newDocumentList
                , lastAst = lastAst
                , renderedText = render docType lastAst
                , message = ( UserMessage, "Document " ++ deletedDocument.title ++ " deleted" )
                , visibilityOfTools = Invisible
              }
            , Cmd.none
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
        , readingModeButton model
        , userPageModeButton model
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
        , showIf (model.appMode == UserMode SignUpState) (inputPasswordConfirmation model)
        , showIf (model.appMode == UserMode SignUpState) (inputEmail model)
        , showIf (model.appMode == UserMode SignUpState) (el [ Font.size 12 ] (Element.text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserMode SignInState) signInButton
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserMode SignUpState) (cancelSignUpButton model)
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
        , signOutButton model
        , showIf (model.appMode == UserMode ChangePasswordState) (passwordPanel model)
        , row [ spacing 12 ]
            [ changePasswordButton model
            , showIf (model.appMode == UserMode ChangePasswordState) (cancelChangePasswordButton model)
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


changePasswordButton : Model -> Element Msg
changePasswordButton model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode ChangePasswordState ->
                    Just ChangePassword

                _ ->
                    Just <| SetAppMode (UserMode ChangePasswordState)
        , label = Element.text "Change password"
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
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Password")
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


signInButton : Element Msg
signInButton =
    Input.button Style.standardButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUpButton : Model -> Element Msg
signUpButton model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode SignUpState ->
                    Just SignUp

                _ ->
                    Just (SetAppMode (UserMode SignUpState))
        , label = Element.text "Sign Up!!"
        }


cancelSignUpButton : Model -> Element Msg
cancelSignUpButton model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode SignUpState ->
                    Just (SetAppMode (UserMode SignInState))

                _ ->
                    Just NoOp
        , label = Element.text "Cancel"
        }


cancelChangePasswordButton : Model -> Element Msg
cancelChangePasswordButton model =
    Input.button Style.standardButton
        { onPress =
            case model.appMode of
                UserMode ChangePasswordState ->
                    Just (SetAppMode (UserMode SignInState))

                _ ->
                    Just NoOp
        , label = Element.text "Cancel"
        }


signOutButton : Model -> Element Msg
signOutButton model =
    Input.button Style.standardButton
        { onPress = Just SignOut
        , label = Element.text "Sign out"
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
                [ row [ spacing 8 ] [ el [ Font.size 14 ] (Element.text "Edit outline below"), setupOutlineButton model, updateChildrenButton model ]
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
            (List.map addSubdocumentButton2 model.candidateChildDocumentList)
        ]


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


xButtonStyle =
    Style.standardButton ++ [ Background.color Style.charcoal, Font.color Style.white ]


setupOutlineButton model =
    Input.button [] { onPress = Just SetUpOutline, label = el xButtonStyle (Element.text "Load") }


updateChildrenButton model =
    Input.button [] { onPress = Just UpdateChildren, label = el xButtonStyle (Element.text "Update") }


getTitles : List Document -> String
getTitles docList =
    List.map .title docList
        |> String.join "\n"


addSubdocumentButton2 : Document -> Element Msg
addSubdocumentButton2 document =
    Input.button Style.activeButtonStyle { onPress = Just (AddThisDocumentToMaster document), label = el [ Font.color Style.white ] (Element.text document.title) }


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
            model.renderedText
    in
    column []
        [ editingHeader viewInfo model rt
        , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, editor viewInfo model, Element.Lazy.lazy (renderedSource viewInfo model footerText) rt ]
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
            model.renderedText
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
    -- XYXY
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

    in
    row [ spacing 10 ]
        [ column [ setElementId "__rt_scroll__", width (px w_), height (px h_), clipX, Font.size 12 ]
            [ column [ width (px w2_), paddingXY 10 20 ]
                [ rt.title |> Element.html
             , column [setElementId masterId,  height (px h_)] [ rt.document |> Element.html ]
                ]
            ]
        , Element.column [ height (px hToc), width (px wToc), Font.size 12, paddingXY 8 0, Background.color (Style.makeGrey 0.9) ]
            [ column [  height (px (hToc - 125)), scrollbarY, clipX ] [ rt.toc |> Element.html ]
            , column [ paddingXY 12 3, width fill, height (px 125), clipX, Background.color (Style.makeGrey 0.5), Font.color (Style.makeGrey 1.0) ]
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


editor : ViewInfo -> Model -> Element Msg
editor viewInfo model =
    let
        w_ =
            affine viewInfo.editorWidth viewInfo.hExtra model.windowWidth |> toFloat

        h_ =
            translate -viewInfo.vInset model.windowHeight |> toFloat
    in
    column []
        [ Element.Keyed.el []
            ( String.fromInt 0
            , editor_ model w_ h_
            )
        ]


pxFromFloat : Float -> String
pxFromFloat f =
    f
        |> round
        |> String.fromInt
        |> (\s -> s ++ "px")


editor_ : Model -> Float -> Float -> Element Msg
editor_ model w_ h_ =
    let
        wpx =
            pxFromFloat w_

        hpx =
            pxFromFloat h_
    in
    Editor.codeEditor
        [ Editor.editorValue (Document.getContent model.currentDocument)
        , Editor.searchTargetValue model.selectedText
        , Editor.onEditorChanged UpdateDocumentText
        , Editor.onGutterClicked ProcessLine
        ]
        []
        |> (\x -> Html.div [ setHtmlId "_editor_",  HA.style "width" wpx, HA.style "height" hpx, HA.style "overflow" "scroll" ] [ x ])
        |> Element.html


userPageModeButton model =
    let
        color =
            case model.appMode of
                UserMode _ ->
                    Style.red

                _ ->
                    Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode (UserMode SignedInState))
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "User"))
        }


editingModeButton model =
    let
        color =
            if model.appMode == Editing StandardEditing then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode (Editing StandardEditing))
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "Edit"))
        }


subDocumentEditingModeButton model =
    let
        color =
            if model.appMode == Editing SubdocumentEditing then
                Style.red

            else
                Style.buttonGrey
    in
    showIf (model.currentUser /= Nothing && List.length model.tableOfContents > 0 && model.documentListType == DocumentChildren)
        (Input.button []
            { onPress = Just (SetAppMode (Editing SubdocumentEditing))
            , label =
                el (headerButtonStyle color)
                    (el headerLabelStyle (Element.text "Edit/C"))
            }
        )


newSubdocumentButton model =
    let
        numberOfChildren =
            Maybe.map (.childInfo >> List.length) model.currentDocument
                |> Maybe.withDefault 0
    in
    showIf (model.appMode == Editing SubdocumentEditing)
        (Input.button
            []
            { onPress = Just NewSubdocument
            , label =
                el []
                    (el (headingButtonStyle 140) (Element.text "New subdocument"))
            }
        )


deleteSubdocumentButton : Model -> Element Msg
deleteSubdocumentButton model =
    let
        numberOfChildren =
            Maybe.map (.childInfo >> List.length) model.currentDocument
                |> Maybe.withDefault 0
    in
    showIf (model.appMode == Editing SubdocumentEditing)
        (Input.button
            []
            { onPress = Just DeleteSubdocument
            , label =
                el []
                    (el (headingButtonStyle 140) (Element.text "Delete subdocument"))
            }
        )


readingModeButton model =
    let
        color =
            if model.appMode == Reading then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode Reading)
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "Read"))
        }


headerButtonStyle color =
    [ height (px 30), width (px 50), Background.color color, Font.color Style.white, Font.size 12 ]


headerLabelStyle =
    [ height (px 30), width (px 80), padding 8 ]



-- DOCUMENT TOOL PANEL: BUTTONS AND INPUTS --


toolPanel viewInfo model =
    let
        h_ =
            translate -viewInfo.vInset model.windowHeight
    in
    column
        [ width (px (scale viewInfo.docListWidth model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.5)
        , paddingXY 20 20
        , alignTop
        ]
        [ column [ Font.size 13, spacing 15 ]
            [ el [ Font.size 16, Font.bold, Font.color Style.white ] (Element.text "Document tools")
            , togglePublic model
            , inputTags model
            , flavors model
            ]
        ]


toolButtonStyleInHeader : List (Element.Attribute msg)
toolButtonStyleInHeader =
    [ height (px 30), width (px 60), padding 8, Background.color (Style.makeGrey 0.1), Border.color Style.white, Font.color Style.white, Font.size 12 ]


inputTags model =
    Input.multiline (Style.textInputStyleSimple 180 80)
        { onChange = GotTagString
        , text = model.tagString
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "Keywords")
        , spellcheck = False
        }


togglePublic model =
    case model.currentDocument of
        Nothing ->
            Element.none

        Just document ->
            case document.public of
                True ->
                    Input.button []
                        { onPress = Just (SetDocumentPublic False)
                        , label = el (headingButtonStyle 140) (Element.text "Public")
                        }

                False ->
                    Input.button []
                        { onPress = Just (SetDocumentPublic True)
                        , label = el (headingButtonStyle 140) (Element.text "Private")
                        }


newDocumentButton model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just CreateDocument
                , label = el toolButtonStyleInHeader (Element.text "New")
                }


firstSubDocumentButton model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just FirstSubdocument
                , label = el toolButtonStyleInHeader (Element.text "New/S")
                }


saveDocumentButton model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            Input.button []
                { onPress = Just SaveDocument
                , label = el toolButtonStyleInHeader (Element.text "Save")
                }


deleteDocumentButton model =
    case model.currentUser of
        Nothing ->
            Element.none

        Just _ ->
            case model.documentDeleteState of
                Armed ->
                    Input.button []
                        { onPress = Just DeleteDocument
                        , label = el (toolButtonStyleInHeader ++ [ Background.color Style.red ]) (Element.text "Delete!")
                        }

                SafetyOn ->
                    Input.button []
                        { onPress = Just ArmForDelete
                        , label = el toolButtonStyleInHeader (Element.text "Delete?")
                        }


cancelDeleteDocumentButtonInHeader model =
    case model.documentDeleteState of
        Armed ->
            Input.button []
                { onPress = Just CancelDeleteDocument
                , label = el (toolButtonStyleInHeader ++ [ Background.color Style.blue ]) (Element.text "Cancel")
                }

        SafetyOn ->
            Element.none



-- DOCUMENT LIST --


docListViewer viewInfo model =
    let
        h_ =
            translate -viewInfo.vInset model.windowHeight

        renderedList =
            case model.documentListType of
                SearchResults ->
                    renderTocForSearchResults model

                DequeView ->
                    renderTocForDeque model

                DocumentChildren ->
                    (expandCollapseTocButton |> Element.map TOC) :: renderTocForMaster model
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
                :: newSubdocumentButton model
                :: deleteSubdocumentButton model
                :: renderedList
            )
        ]



-- TABLE OF CONTENTS


renderTocForSearchResults : Model -> List (Element Msg)
renderTocForSearchResults model =
    List.map (tocEntry model.currentDocument) model.documentList


renderTocForDeque: Model -> List (Element Msg)
renderTocForDeque model =
    List.map (tocEntry model.currentDocument) (BoundedDeque.toList model.deque)

renderTocForMaster : Model -> List (Element Msg)
renderTocForMaster model =
    case model.tocData of
        Nothing ->
            [ el [] (Element.text <| "Loading TOC ...") ]

        Just zipper ->
            [ viewZ model.toggleToc zipper |> Element.map TOC ]


expandCollapseTocButton =
    Input.button (Style.activeButtonStyle ++ [ Font.size 12 ])
        { onPress = Just Toggle
        , label = Element.text "Expand/Collapse"
        }


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        ( color, fontWeight ) =
            tocEntryStyle currentDocument_ document
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [ Font.color color, fontWeight ] (Element.text document.title) }


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


headingButtonStyle w =
    [ height (px 30), width (px w), padding 8, Background.color Style.charcoal, Font.color Style.white, Font.size 12 ]


heading : Model -> Element Msg
heading model =
    let
        n_ =
            case model.documentListType of
                SearchResults ->
                    List.length model.documentList

                DocumentChildren ->
                    List.length model.tableOfContents

                DequeView ->
                   BoundedDeque.length model.deque

        n =
            n_
                |> String.fromInt

        w =
            140
    in
    case model.currentUser of
        Nothing ->
            case model.documentListType of
                SearchResults ->
                    Input.button []
                        { onPress = Just (SetDocumentListType DocumentChildren)
                        , label =
                            el (headingButtonStyle w)
                                (Element.text ("Public Documents (" ++ n ++ ")"))
                        }

                DocumentChildren ->
                    Input.button []
                        { onPress = Just (SetDocumentListType SearchResults)
                        , label =
                            el (headingButtonStyle w)
                                (Element.text ("Contents (" ++ n ++ ")"))
                        }

                DequeView ->
                    Input.button []
                        { onPress = Just (SetDocumentListType DequeView)
                        , label =
                            el (headingButtonStyle w)
                                (Element.text ("Recent (" ++ n ++ ")"))
                      }

        Just _ ->
            case model.documentListType of
                SearchResults ->
                    row [ spacing 10 ] [ setDocumentListTypeButton w n, sortByMostRecentFirstButton model, sortAlphabeticalButton model, setDequeViewButton ]

                DocumentChildren ->
                    row [ spacing 10 ] [setDocumentChildrenButton w n, setDequeViewButtonX w n]

                DequeView ->
                    row [ spacing 10 ] [ setDocumentListTypeButton w n, sortByMostRecentFirstButton model, sortAlphabeticalButton model, setDequeViewButton ]


setDequeViewButtonX w n =
    Input.button []
        { onPress = Just (SetDocumentListType DequeView)
        , label =
            el (headingButtonStyle w)
                (Element.text ("Recent (" ++ n ++ ")"))
        }

setDocumentChildrenButton w n =
    Input.button []
        { onPress = Just (SetDocumentListType SearchResults)
        , label =
            el (headingButtonStyle w)
                (Element.text ("Contents (" ++ n ++ ")"))
                }



setDocumentListTypeButton w n =
    Input.button []
        { onPress = Just (SetDocumentListType DocumentChildren)
        , label =
            el (headingButtonStyle w)
                (Element.text ("Documents (" ++ n ++ ")"))
        }

setDequeViewButton  =
    Input.button []
        { onPress = Just (SetDocumentListType DequeView)
        , label =
            el (headingButtonStyle 60)
                (Element.text "Recent")
        }


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
            [ titleRowForEditing titleWidth rt
            , searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]


searchRow model =
    row [ spacing 10, alignRight ] [ inputSearchTerms model, clearSearchTermsButton, searchButton model, allDocumentsButton, helpDocsButton ]


titleRowForEditing titleWidth rt =
    row [ Font.size 12, height (px 40), width (px titleWidth), Font.color Style.white, alignRight, clipX ]
        [ row [ alignRight, clipX, moveUp 12 ] [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ] ]


titleRow titleWidth rt =
    row [ Font.size 12, height (px 40), width (px titleWidth), Font.color Style.white, alignRight, clipX ]
        [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ]


editTools : Model -> Element Msg
editTools model =
    if List.member model.appMode [ Editing StandardEditing, Editing SubdocumentEditing ] then
        row [ spacing 6 ]
            [ editingModeButton model
            , subDocumentEditingModeButton model
            , newDocumentButton model
            , firstSubDocumentButton model
            , saveDocumentButton model
            , deleteDocumentButton model
            , cancelDeleteDocumentButtonInHeader model
            ]

    else
        row [ spacing 6 ] [ editingModeButton model, subDocumentEditingModeButton model ]



-- TAB-STRIP ON LEFT --


tabStrip : ViewInfo -> Model -> Element Msg
tabStrip viewInfo model =
    column [ width (px 30), height (px 200), Background.color (Style.grey 0.1), alignTop ]
        [ row [ spacing 15, rotate -1.5708, moveLeft 50, moveDown 70 ] [ showToolsButton model, showDocumentListButton model ]
        ]


showToolsButton : Model -> Element Msg
showToolsButton model =
    let
        color =
            if model.visibilityOfTools == Visible then
                Style.red

            else
                Style.buttonGrey
    in
    case model.appMode of
        Editing StandardEditing ->
            Input.button []
                { onPress = Just (SetToolPanelState Visible)
                , label = el [ height (px 30), width (px 50), padding 8, Background.color color, Font.color Style.white, Font.size 12 ] (Element.text "Tools")
                }

        _ ->
            Input.button []
                { onPress = Just NoOp
                , label = el [ height (px 30), width (px 50), padding 8, Background.color Style.charcoal, Font.color Style.white, Font.size 12 ] (Element.text "")
                }


showDocumentListButton : { a | visibilityOfTools : Visibility } -> Element Msg
showDocumentListButton model =
    let
        color =
            if model.visibilityOfTools == Invisible then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetToolPanelState Invisible)
        , label = el [ height (px 30), padding 8, Background.color color, Font.color Style.white, Font.size 12 ] (Element.text "Documents")
        }



-- FOOTER --


footer : Model -> Element Msg
footer model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ currentAuthorDisplay model
        -- , el [] (Element.text <| slugOfCurrentDocument model)
        , getTextSelectionButton
        , dirtyDocumentDisplay model
        , wordCount model
        , row [ spacing 4 ] [ totalWordCountButton, totalWordCountDisplay model ]
        , displayMessage model.message
        , currentTime model
        ]

displayMessage : Message -> Element Msg
displayMessage (messageType, str) =
    case messageType of
        ErrorMessage -> el [Font.color Style.white, Background.color (Style.brightRed),  alignRight, Font.size 12, Font.bold,  paddingXY 10 4, centerY] (Element.text str)
        _ -> el [alignRight, paddingXY 10 0] (Element.text str)


getTextSelectionButton =
    Input.button [] {
       onPress = Just GetTextSelection
       , label = el [] (Element.text "Sync L <- R")
    }


totalWordCountButton =
    Input.button []
        { onPress = Just DoTotalWordCount
        , label = el [] (Element.text "Total word count: ")
        }


totalWordCountDisplay model =
    if model.flashCount == 0 then
        Element.none

    else
        let
            words =
                model.totalWordCount

            pages =
                Basics.round <| toFloat words / 300.0

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
        (Element.text <| "Current time: " ++ Utility.humanTimeHM model.zone model.time)


wordCount model =
    let
        sourceText =
            case model.currentDocument of
                Just document ->
                    document.content

                _ ->
                    ""

        wc =
            Utility.wordCount sourceText
    in
    Element.el [ Font.color Style.white, Font.size 12 ] (Element.text <| "Word count: " ++ wc)


status model =
    el [ Font.color Style.white, Font.size 12, centerX ]
        (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)


flavors model =
    let
        w =
            120
    in
    column [ spacing 0 ]
        [ el [ Font.color Style.white, Font.bold, paddingXY 0 8 ] (Element.text "Document Type")
        , miniLaTeXButton model w
        , collectionButton model w
        , standardMarkdownButton model w
        , extendedMarkdownButton model w
        , extendedMathMarkdownButton model w
        ]



---- BUTTONS --


miniLaTeXButton model width =
    let
        bit =
            model.docType == MiniLaTeX
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType MiniLaTeX), label = el [ paddingXY 8 0 ] (Element.text "MiniLaTeX") }


collectionButton model width =
    let
        bit =
            model.docType == Collection
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType Collection), label = el [ paddingXY 8 0 ] (Element.text "Collection") }


standardMarkdownButton model width =
    let
        bit =
            model.docType == Markdown MDStandard
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDStandard)), label = el [ paddingXY 8 0 ] (Element.text "Markdown standard") }


extendedMarkdownButton model width =
    let
        bit =
            model.docType == Markdown MDExtended
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDExtended)), label = el [ paddingXY 8 0 ] (Element.text "Markdown extended") }


extendedMathMarkdownButton model width =
    let
        bit =
            model.docType == Markdown MDExtendedMath
    in
    Input.button (Style.buttonSelected width bit)
        { onPress = Just (SetDocType (Markdown MDExtendedMath)), label = el [ paddingXY 8 0 ] (Element.text "Markdown math") }



-- VIEW UTILITIES: showIf, etc.


showIf : Bool -> Element Msg -> Element Msg
showIf bit element =
    if bit then
        element

    else
        Element.none


hideIf : Bool -> Element Msg -> Element Msg
hideIf bit element =
    if not bit then
        element

    else
        Element.none


showOne : Bool -> String -> String -> String
showOne bit str1 str2 =
    case bit of
        True ->
            str1

        False ->
            str2
