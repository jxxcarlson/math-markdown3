module Model exposing
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
    , debounceConfig
    , editorConfig
    )

import Api.InputObject exposing (Document_order_by(..))
import BoundedDeque exposing (BoundedDeque)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Buffer exposing (Buffer)
import Debounce exposing (Debounce)
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Editor exposing (EditorConfig, PEEditorMsg, State)
import Editor.Config exposing (WrapOption(..))
import File exposing (File)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html
import Html.Attributes as HA
import Keyboard exposing (Key(..))
import Outside
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended exposing (Seed)
import Render exposing (RenderingData)
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..))
import SingleSlider as Slider
import Time exposing (Posix)
import Toc exposing (TocItem)
import TocZ exposing (TocMsg(..))
import Tree.Zipper exposing (Zipper)
import Url exposing (Url)
import User exposing (User)


type alias Model =
    { seed : Int
    , debounce : Debounce String

    -- NAV
    , key : Nav.Key
    , url : Url.Url

    -- UI
    , docType : Document.DocType
    , windowWidth : Int
    , windowHeight : Int
    , visibilityOfTools : Visibility
    , appMode : AppMode
    , documentListDisplay : DocumentListDisplay
    , message : Message
    , pressedKeys : List Key
    , focusedElement : FocusedElement
    , flashCounterForTotalWordCount : Int
    , flashCounterForShareUrl : Int

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
    , editorTargetLineNumber : Maybe Int

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
    , renderingData : RenderingData Msg
    , tagString : String
    , searchTerms : String
    , sortTerm : OptionalArgument (List Document_order_by)
    , searchMode : SearchMode
    , sortMode : SortMode
    , documentOutline : String
    , usernameToAddToPermmission : String
    , permissionToAdd : Permission

    -- Editor
    , editorBuffer : Buffer
    , editorState : State
    }


editorConfig =
    { editorMsg = EditorMsg
    , sliderMsg = SliderMsg
    , editorStyle = editorStyle
    , width = 500
    , lines = 50
    , lineHeight = 14.0
    , showInfoPanel = True
    , wrapParams = { maximumWidth = 55, optimalWidth = 50, stringWidth = String.length }
    , wrapOption = DontWrap
    }


editorStyle : List (Html.Attribute msg)
editorStyle =
    [ HA.style "background-color" "#dddddd"
    , HA.style "border" "solid 0.5px"
    ]


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 10000
    , transform = DebounceMsg
    }


type Visibility
    = Visible
    | Invisible


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


type alias DocumentListDisplay =
    ( DocumentListType, DequeViewState )


type DocumentListType
    = SearchResults
    | DocumentChildren


type DequeViewState
    = DequeViewOn
    | DequeViewOff


type SearchMode
    = UserSearch
    | PublicSearch
    | SharedDocSearch


type FocusedElement
    = FocusOnSearchBox
    | NoFocus


type DocumentDeleteState
    = SafetyOn
    | Armed


type SearchType
    = TitleSearch
    | KeywordSearch
    | NoSearchTerm



-- MSG --


type Msg
    = NoOp
      -- System
    | NewUuid
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | DebounceMsg Debounce.Msg
      -- | FeedDebouncer String
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
    | UpdateDocumentText String
    | ProcessLine String
    | SyncEditorToLine Int
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
    | SetDequeview DequeViewState
    | ToggleDequeview
    | SetDocType DocType
    | SetCurrentDocument Document
    | DownloadArchive
    | ArchiveRequested
    | ArchiveSelected File
    | ArchiveLoaded String
    | SaveImportedArchive
      -- Subdocuments
    | SetCurrentSubDocument Document TocItem
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
    | DoShareUrl
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
    | SetDocumentPublic Bool
    | GotSecondPart (RenderingData Msg)
    | GotTagString String
    | Clear
    | Req RequestMsg
    | TOC TocMsg
      -- Doc Permissions
    | AddUserNameForPermissions String
    | CyclePermission
    | AddUserPermission
      -- Editor
    | EditorMsg PEEditorMsg
    | SliderMsg Slider.Msg
