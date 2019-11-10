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
    , RenderedText
    , SearchMode(..)
    , SearchType(..)
    , SortMode(..)
    , UserState(..)
    , Visibility(..)
    , debounceConfig
    )

import Api.InputObject exposing (Document_order_by(..))
import BoundedDeque exposing (BoundedDeque)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Debounce exposing (Debounce)
import Document exposing (DocType(..), Document, MarkdownFlavor(..), Permission(..))
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html)
import Keyboard exposing (Key(..))
import Outside
import ParseWithId
import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended exposing (Seed)
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..))
import Time exposing (Posix)
import Toc exposing (TocItem)
import TocZ exposing (TocMsg(..))
import Tree exposing (Tree)
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
    , lastAst : Tree ParseWithId.MDBlockWithId
    , renderedText : RenderedText Msg
    , tagString : String
    , searchTerms : String
    , sortTerm : OptionalArgument (List Document_order_by)
    , searchMode : SearchMode
    , sortMode : SortMode
    , documentOutline : String
    , usernameToAddToPermmission : String
    , permissionToAdd : Permission
    }


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 200
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


type alias RenderedText msg =
    { title : Html msg, toc : Html msg, document : Html msg }



-- MSG --


type Msg
    = NoOp
      -- System
    | NewUuid
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | DebounceMsg Debounce.Msg
    | FeedDebouncer String
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
    | GotSecondPart (RenderedText Msg)
    | GotTagString String
    | Clear
    | Req RequestMsg
    | TOC TocMsg
      -- Doc Permissions
    | AddUserNameForPermissions String
    | CyclePermission
    | AddUserPermission
