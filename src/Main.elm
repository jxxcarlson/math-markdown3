module Main exposing (main, parseSearchTerm)

import Browser
import Browser.Dom
import Browser.Events
import Data
import Document exposing (DocType(..), Document, MarkdownFlavor(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import Html exposing (..)
import Html.Attributes as HA
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import List.Extra
import Markdown.Elm
import Markdown.ElmWithId
import Markdown.Option exposing (Option(..))
import ParseWithId
import Prng.Uuid as Uuid
import Process
import Random
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData(..))
import Request exposing (GraphQLResponse(..), RequestMsg(..))
import Style
import Task
import Time exposing (Posix)
import Toc exposing (TocItem)
import Tree exposing (Tree)
import Tree.Diff as Diff
import User exposing (User)
import Utility


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }



-- MODEL --


type alias RenderedText msg =
    { title : Html msg, toc : Html msg, document : Html msg }


type alias Model =
    { seed : Int

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

    -- SYSTEM
    , currentSeed : Seed
    , currentUuid : Uuid.Uuid
    , zone : Time.Zone
    , time : Time.Posix

    -- USER
    , currentUser : Maybe User
    , username : String
    , email : String
    , password : String
    , newPassword1 : String
    , newPassword2 : String

    -- DOCUMENT
    , counter : Int
    , documentDeleteState : DocumentDeleteState
    , documentList : List Document
    , childDocumentList : List Document
    , tocTree : Maybe (Tree TocItem)
    , currentTocLabel : Maybe TocItem
    , candidateChildDocumentList : List Document
    , childDocIdString : String
    , currentDocument : Maybe Document
    , currentDocumentDirty : Bool
    , secondsWhileDirty : Int
    , lastAst : Tree ParseWithId.MDBlockWithId
    , renderedText : RenderedText Msg
    , tagString : String
    , searchTerms : String
    , searchMode : SearchMode
    , documentOutline : String
    }


hasuraToken : String
hasuraToken =
    "GOc97wA7CCMm31H4UJHa-4pqdVoLf3l6gAwzczdHC"


type DocumentListType
    = SearchResults
    | DocumentChildren


type SearchMode
    = UserSearch
    | PublicSearch


type alias Message =
    ( MessageType, String )


type MessageType
    = SignInMessage
    | UserMessage
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


appStateAsString : Model -> String
appStateAsString model =
    case model.appMode of
        Reading ->
            "Reading"

        Editing StandardEditing ->
            "Editing"

        Editing SubdocumentEditing ->
            "Editing subdocuments"

        UserMode SignInState ->
            "Signing in"

        UserMode SignUpState ->
            "Signing up"

        UserMode ChangePasswordState ->
            "Changing Password"

        UserMode SignedInState ->
            "Signed in"


type DocumentDeleteState
    = SafetyOn
    | Armed


type Visibility
    = Visible
    | Invisible



-- INIT --


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( newUuid, newSeed ) =
            step Uuid.generator (initialSeed flags.seed flags.randInts)

        model : Model
        model =
            { seed = 0

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

            -- SYSTEM
            , currentSeed = newSeed -- initialSeed flags.seed flags.randInts
            , currentUuid = newUuid -- Nothing
            , zone = Time.utc
            , time = Time.millisToPosix 0

            -- USER
            , currentUser = Nothing
            , username = ""
            , email = ""
            , password = ""
            , newPassword1 = ""
            , newPassword2 = ""

            -- documents
            , counter = 0
            , documentDeleteState = SafetyOn
            , documentList = [ Data.loadingPage ]
            , childDocumentList = []
            , tocTree = Nothing
            , currentTocLabel = Nothing
            , candidateChildDocumentList = []
            , childDocIdString = ""
            , currentDocument = Nothing
            , currentDocumentDirty = False
            , secondsWhileDirty = 0
            , lastAst = emptyAst
            , renderedText = emptyRenderedText
            , tagString = ""
            , searchTerms = ""
            , searchMode = PublicSearch
            , documentOutline = ""
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Request.publicDocuments hasuraToken |> Cmd.map Req
        ]
    )



-- MSG --


type Msg
    = NoOp
      -- System
    | NewUuid
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
      -- Random
    | GenerateSeed
    | NewSeed Int
      -- UI
    | SetToolPanelState Visibility
    | SetAppMode AppMode
    | WindowSize Int Int
    | KeyMsg Keyboard.Msg
    | SetFocusOnSearchBox (Result Browser.Dom.Error ())
      -- User
    | GotUserName String
    | GotPassword String
    | GotNewPassword1 String
    | GotNewPassword2 String
    | ChangePassword
    | GotEmail String
    | SignIn
    | SignUp
    | SignOut
      -- Document
    | CreateDocument
    | SaveDocument
    | GetUserDocuments
    | AllDocuments
    | GetPublicDocuments
    | GetHelpDocs
    | SetDocumentListType DocumentListType
    | SetDocType DocType
    | SetCurrentDocument Document
    | SetCurrentSubDocument Document TocItem
      -- Subdocuments
    | NewSubdocument
    | AddSubdocument
    | DeleteSubdocument
    | SetUpOutline
    | UpdateChildren
    | GotOutline String
    | AddThisDocumentToMaster Document
    | GotChildDocIdString String
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


type alias Flags =
    { width : Int
    , height : Int
    , seed : Int
    , randInts : List Int
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


vInset =
    75



-- 208


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


scale : Float -> Int -> Int
scale factor input =
    factor * toFloat input |> round


affine : Float -> Float -> Int -> Int
affine factor shift input =
    factor * (toFloat input - shift) |> round


translate : Float -> Int -> Int
translate amount input =
    toFloat input + amount |> round


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onResize WindowSize
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

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

        GotNewPassword1 _ ->
            ( model, Cmd.none )

        GotNewPassword2 _ ->
            ( model, Cmd.none )

        ChangePassword ->
            ( model, Cmd.none )

        GotEmail _ ->
            ( model, Cmd.none )

        SignIn ->
            signIn model

        SignUp ->
            ( { model | appMode = UserMode SignUpState, message = ( DebugMessage, "At SignUp msg" ) }, Cmd.none )

        SignOut ->
            ( { model
                | currentUser = Nothing
                , appMode = UserMode SignInState
                , username = ""
                , password = ""
                , currentDocument = Nothing
                , documentList = []
              }
            , Request.publicDocuments hasuraToken |> Cmd.map Req
            )

        -- DOCUMENT --
        CreateDocument ->
            makeNewDocument model

        NewSubdocument ->
            newSubdocument model

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
            updateDocumentText model str

        SetCurrentDocument document ->
            processDocument model document

        SetCurrentSubDocument document tocItem ->
            setCurrentSubdocument model document tocItem

        SetUpOutline ->
            ( setupOutline model, Cmd.none )

        UpdateChildren ->
            case model.currentDocument of
                Nothing ->
                    ( model, Cmd.none )

                Just masterDocument ->
                    let
                        titleList =
                            String.split "\n" model.documentOutline
                                |> List.map String.trim
                                |> List.filter (\str -> str /= "")

                        newMasterDocument_ =
                            Document.reorderChildren masterDocument titleList (List.drop 1 model.childDocumentList)

                        newMasterDocument =
                            Document.setLevelsOfChildren model.documentOutline newMasterDocument_

                        newChildDocumentList =
                            newMasterDocument
                                :: List.drop 1 model.childDocumentList
                                |> Document.sortChildren newMasterDocument
                    in
                    ( { model
                        | currentDocument = Just newMasterDocument
                        , childDocumentList = newChildDocumentList
                        , tocTree = Just <| Toc.make newMasterDocument (List.drop 1 newChildDocumentList)
                      }
                    , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
                    )

        SetDocumentPublic bit ->
            setDocumentPublic model bit

        GotTagString str ->
            processTagString model str

        GotChildDocIdString str ->
            ( { model | childDocIdString = str }, Cmd.none )

        GotSearchTerms str ->
            ( { model | searchTerms = str, focusedElement = FocusOnSearchBox }, Cmd.none )

        DoSearch ->
            doSearch model

        ToggleSearchMode ->
            toggleSearchMode model

        ClearSearchTerms ->
            clearSearchTerms model

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



-- UPDATE HELPERS --
-- KEYBOARD HELPERS --


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



-- SYSTEM HELPERS


handleTime : Model -> Posix -> ( Model, Cmd Msg )
handleTime model newTime =
    let
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
    ( { model | time = newTime, secondsWhileDirty = secondsWhileDirty }
    , cmd
    )



-- USER HELPERS


signIn : Model -> ( Model, Cmd Msg )
signIn model =
    if
        (model.username == "jxxcarlson" && model.password == "locoLobo")
            || (model.username == "boris" && model.password == "locoLobo")
            || (model.username == "cervone" && model.password == "mathjax3")
    then
        ( { model
            | currentUser = Just (User.dummy model.username)
            , appMode = Reading
            , visibilityOfTools = Invisible
            , searchMode = UserSearch
          }
        , Request.documentsWithAuthor hasuraToken model.username |> Cmd.map Req
        )

    else
        ( { model | currentUser = Nothing, appMode = UserMode SignInState, password = "" }, Cmd.none )



-- SEARCH HELPERS


type SearchType
    = TitleSearch
    | KeywordSearch
    | NoSearchTerm


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
    Input.text (Style.inputStyle 200 ++ [ Element.htmlAttribute <| HA.attribute "id" "search-box" ])
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
    Task.attempt SetFocusOnSearchBox (Browser.Dom.focus "search-box")


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
                            Request.documentsWithAuthorAndTitle hasuraToken user.username "" GotUserDocuments |> Cmd.map Req

                PublicSearch ->
                    Request.publicDocumentsWithTitle hasuraToken "" |> Cmd.map Req
    in
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


getHelpDocs : Model -> ( Model, Cmd Msg )
getHelpDocs model =
    ( { model | documentListType = SearchResults, focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }
    , Request.publicDocumentsWithTag hasuraToken "help" |> Cmd.map Req
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
                    Request.documentsWithAuthorAndTitle hasuraToken authorIdentifier searchTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTag hasuraToken authorIdentifier searchTerm GotUserDocuments |> Cmd.map Req

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
                    Request.documentsWithAuthorAndTitle hasuraToken authorIdentifier searchTerm GotCandidateChildDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTag hasuraToken authorIdentifier searchTerm GotCandidateChildDocuments |> Cmd.map Req

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



-- END SEARCH
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
        |> Task.andThen (\_ -> Process.sleep 100 |> Task.andThen (\_ -> Task.succeed (Markdown.ElmWithId.renderHtmlWithExternaTOC ast)))
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

                        nMath =
                            Markdown.ElmWithId.numberOfMathElements lastAst

                        ( renderedText, cmd_ ) =
                            if nMath > 10 then
                                let
                                    firstAst =
                                        Markdown.ElmWithId.parse (model.counter + 1) ExtendedMath (getFirstPart content)

                                    renderedText_ =
                                        render doc.docType firstAst

                                    cmd__ =
                                        renderAstFor lastAst
                                in
                                ( renderedText_
                                , cmd__
                                )

                            else
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
    -- XXX
    let
        ( newDocumentList, tocTree ) =
            case model.currentDocument of
                Nothing ->
                    ( documentList, Nothing )

                Just masterDocument ->
                    let
                        sortedChildDocuments =
                            Document.sortChildren masterDocument documentList
                    in
                    ( masterDocument :: sortedChildDocuments
                    , Just <| Toc.make masterDocument sortedChildDocuments
                    )
    in
    ( { model
        | childDocumentList = newDocumentList
        , tocTree = tocTree
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


processDocument : Model -> Document -> ( Model, Cmd Msg )
processDocument model document =
    -- XXX
    let
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
                        Markdown.ElmWithId.renderHtmlWithExternaTOC <| firstAst

                    else
                        Markdown.ElmWithId.renderHtmlWithExternaTOC lastAst

                cmd1 =
                    if nMath > 10 then
                        renderAstFor lastAst

                    else
                        Cmd.none

                ( cmd2, documentListType_ ) =
                    if document.children == [] then
                        ( Cmd.none, model.documentListType )

                    else
                        ( Request.documentsInIdList hasuraToken document.children |> Cmd.map Req, DocumentChildren )
            in
            ( ( lastAst, renderedText ), Cmd.batch [ cmd1, cmd2 ], documentListType_ )
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
        currentTocLabel =
            case tocItem.hasChildren of
                -- case tocItem.level < 1 of
                Just True ->
                    Just tocItem

                Just False ->
                    model.currentTocLabel

                Nothing ->
                    model.currentTocLabel

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
                        Markdown.ElmWithId.renderHtmlWithExternaTOC <| firstAst

                    else
                        Markdown.ElmWithId.renderHtmlWithExternaTOC lastAst

                cmd1 =
                    if nMath > 10 then
                        renderAstFor lastAst

                    else
                        Cmd.none

                ( cmd2, documentListType_ ) =
                    if document.children == [] then
                        ( Cmd.none, model.documentListType )

                    else
                        ( Request.documentsInIdList hasuraToken document.children |> Cmd.map Req, DocumentChildren )
            in
            ( ( lastAst, renderedText ), Cmd.batch [ cmd1, cmd2 ], documentListType_ )
    in
    ( { model
        | currentDocument = Just document
        , currentTocLabel = currentTocLabel -- XXXX
        , documentListType = documentListType
        , counter = model.counter + 2
        , lastAst = newAst
        , renderedText = newRenderedText
        , tagString = document.tags |> String.join ", "
        , message = ( UserMessage, "Success getting document list" )
      }
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
                , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC lastAst
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
    case ( List.head model.childDocumentList, model.currentDocument ) of
        ( Just masterDocument, Just documentToDelete ) ->
            deleteSubdocument_ model masterDocument documentToDelete

        ( _, _ ) ->
            ( model, Cmd.none )


deleteSubdocument_ : Model -> Document -> Document -> ( Model, Cmd Msg )
deleteSubdocument_ model masterDocument documentToDelete =
    -- XXX
    let
        newMasterDocument_ =
            Document.deleteChild documentToDelete masterDocument

        indexOfChildToDelete =
            List.Extra.elemIndex documentToDelete.id masterDocument.children

        newChildLevels =
            case indexOfChildToDelete of
                Nothing ->
                    masterDocument.childLevels

                Just idx ->
                    List.Extra.removeAt idx masterDocument.childLevels

        newMasterDocument =
            { newMasterDocument_ | childLevels = newChildLevels }

        newDocumentList =
            List.filter (\doc -> doc.id /= documentToDelete.id) model.documentList
                |> Document.replaceInList newMasterDocument

        newChildDocumentList =
            List.filter (\doc -> doc.id /= documentToDelete.id) model.childDocumentList
                |> Document.replaceInList newMasterDocument

        newDocumentOutline =
            case computeOutline newMasterDocument newChildDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline
    in
    ( { model
        | currentDocument = Just newMasterDocument
        , childDocumentList = newChildDocumentList
        , tocTree = Just <| Toc.make newMasterDocument (Document.sortChildren newMasterDocument newChildDocumentList)
        , documentList = newDocumentList
        , documentOutline = newDocumentOutline
      }
    , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
    )


addSubdocument_ : Model -> User -> Document -> Uuid.Uuid -> ( Model, Cmd Msg )
addSubdocument_ model user masterDocument newUuid =
    let
        newChildren =
            masterDocument.children
                ++ [ newUuid ]

        newMasterDocument =
            { masterDocument | children = newChildren }
    in
    ( { model | currentDocument = Just newMasterDocument, appMode = Reading }
    , Cmd.batch
        [ Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList hasuraToken newChildren |> Cmd.map Req
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
        newChildren =
            master.children
                ++ [ document.id ]

        newMasterDocument =
            { master | children = newChildren }
    in
    ( { model | currentDocument = Just newMasterDocument }
    , Cmd.batch
        [ Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        , Request.documentsInIdList hasuraToken newChildren |> Cmd.map Req
        ]
    )


newSubdocument : Model -> ( Model, Cmd Msg )
newSubdocument model =
    case ( model.currentUser, List.head model.childDocumentList, model.currentDocument ) of
        ( Just user, Just masterDocument, Just currentDocument ) ->
            newSubdocument_ model user masterDocument currentDocument

        ( _, _, _ ) ->
            ( model, Cmd.none )


newSubdocument_ : Model -> User -> Document -> Document -> ( Model, Cmd Msg )
newSubdocument_ model user masterDocument targetDocument =
    --- XXX
    let
        newDocumentText =
            "# New subdocument of " ++ masterDocument.title ++ "\n\nWrite something here ..."

        newDocument =
            Document.create model.currentUuid user.username "New Subdocument" newDocumentText

        lastAst =
            Markdown.ElmWithId.parse -1 ExtendedMath newDocumentText

        ( newUuid, newSeed ) =
            step Uuid.generator model.currentSeed

        --        newChildren =
        --            masterDocument.children ++ [ newDocument.id ]
        targetIndex =
            List.Extra.elemIndex targetDocument.id masterDocument.children |> Maybe.withDefault 0

        newChildren =
            Utility.insertUuidInList newDocument.id targetDocument.id masterDocument.children

        --        newLevels =
        --            masterDocument.childLevels ++ [ 0 ]
        newLevels =
            Utility.insertIntegerAtIndex 0 targetIndex masterDocument.childLevels
                |> List.take (List.length newChildren)

        -- ensure that the lists have the same lengtht
        newMasterDocument =
            { masterDocument | children = newChildren, childLevels = newLevels }

        newChildDocumentList =
            Document.insertDocumentInList newDocument targetDocument model.childDocumentList
                |> Document.replaceInList newMasterDocument
                |> Document.sortChildren newMasterDocument

        newDocumentList =
            Document.replaceInList newMasterDocument (newDocument :: model.documentList)

        newDocumentOutline =
            case computeOutline newMasterDocument newChildDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline
    in
    ( { model
        | currentDocument = Just newMasterDocument
        , childDocumentList = newChildDocumentList
        , tocTree = Just <| Toc.make newMasterDocument newChildDocumentList
        , documentList = newDocumentList
        , documentListType = DocumentChildren
        , documentOutline = newDocumentOutline
        , visibilityOfTools = Invisible
        , appMode = Editing SubdocumentEditing
        , tagString = ""
        , currentUuid = newUuid
        , currentSeed = newSeed
        , message = ( UserMessage, "subdocument added" )
        , lastAst = lastAst
        , renderedText = Markdown.ElmWithId.renderHtmlWithExternaTOC lastAst
      }
    , Cmd.batch
        [ Request.insertDocument hasuraToken newDocument |> Cmd.map Req
        , Request.updateDocument hasuraToken newMasterDocument |> Cmd.map Req
        ]
    )


updateDocumentText : Model -> String -> ( Model, Cmd Msg )
updateDocumentText model str =
    --- XXX
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
            in
            ( { model
                | -- document
                  currentDocument = Just updatedDoc2
                , documentList = Document.replaceInList updatedDoc2 model.documentList
                , childDocumentList = Document.replaceInList updatedDoc2 model.childDocumentList
                , currentDocumentDirty = True

                -- rendering
                , lastAst = newAst
                , renderedText = render updatedDoc2.docType newAst
                , counter = model.counter + 1
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
    Markdown.ElmWithId.renderHtmlWithExternaTOC ast


markdownOptionOfFlavor : MarkdownFlavor -> Markdown.Option.Option
markdownOptionOfFlavor flavor =
    case flavor of
        MDStandard ->
            Standard

        MDExtended ->
            Extended

        MDExtendedMath ->
            ExtendedMath


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



-- UI HELPERS


setModeToReading : Model -> ( Model, Cmd Msg )
setModeToReading model =
    ( { model | appMode = Reading, visibilityOfTools = Invisible }, Cmd.none )


setModeToEditing : Model -> EditMode -> ( Model, Cmd Msg )
setModeToEditing model editMode =
    let
        visibility =
            case model.documentListType of
                SearchResults ->
                    Visible

                DocumentChildren ->
                    Invisible
    in
    ( { model
        | appMode = Editing editMode
        , visibilityOfTools = visibility
        , documentOutline = setupOutline_ model
      }
    , Cmd.none
    )


setUserMode : Model -> UserState -> ( Model, Cmd msg )
setUserMode model s =
    case model.currentUser of
        Nothing ->
            ( { model | appMode = UserMode SignInState }, Cmd.none )

        Just _ ->
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


type alias RenderedDocumentRecord msg =
    { document : Html msg, title : Html msg, toc : Html msg }


view : Model -> Html Msg
view model =
    case model.appMode of
        Reading ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (readingDisplay viewInfoReading model)

        Editing StandardEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (editingDisplay viewInfoEditing model)

        Editing SubdocumentEditing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (subdocumentEditor viewInfoEditingSubdocuemnt model)

        UserMode _ ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (userPageDisplay viewInfoUserPage model)



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


viewInfoUserPage =
    { lhsFraction = 0.45
    , rhsFraction = 0.55
    , vInset = vInset
    }


userPageDisplay : ViewInfoUserPage -> Model -> Element Msg
userPageDisplay viewInfo model =
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
        [ column [ width (px w1), height (px h), padding 36, scrollbarY, Background.color (Style.makeGrey 0.9), Font.color (Style.makeGrey 0.1) ]
            [ rt.title |> Element.html, rt.document |> Element.html ]
        , column [ width (px w2), height (px h), padding 12, scrollbarY, Background.color (Style.makeGrey 0.8), Font.color (Style.makeGrey 0.1) ]
            [ rt.toc |> Element.html ]
        ]


userPageFooter : Model -> Element Msg
userPageFooter model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ el [] (Element.text <| appStateAsString model)
        , el [] (Element.text <| (model.message |> Tuple.second))
        ]


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



-- SIGN-IN


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
        , showIf (model.appMode == UserMode SignUpState) (inputEmail model)
        , showIf (model.appMode == UserMode SignUpState) (el [ Font.size 12 ] (Element.text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserMode SignInState) signInButton
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserMode SignUpState) (cancelSignUpButton model)
                ]
            ]
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


config =
    { debounceInterval = 500
    , timeoutInMs = 5 * 1000
    , panelHeight = 550
    , panelWidth = 450
    }



-- VIEW: DISPLAY RENDERED TEXT --
-- SUBDOCUMENT EDITOR


subdocumentEditor : ViewInfo -> Model -> Element Msg
subdocumentEditor viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "FOOTER"
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

        -- XXX
        , footer model
        ]



-- XXX


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



-- XXX: Deprecated


setupOutline : Model -> Model
setupOutline model =
    { model | documentOutline = setupOutline_ model }


setupOutline_ : Model -> String
setupOutline_ model =
    case model.currentDocument of
        Just currentDoc ->
            case computeOutline currentDoc model.childDocumentList of
                Nothing ->
                    model.documentOutline

                Just outline ->
                    outline

        Nothing ->
            model.documentOutline


computeOutline : Document -> List Document -> Maybe String
computeOutline currentDoc childDocumentList =
    case Just currentDoc.id == Maybe.map .id (List.head childDocumentList) of
        False ->
            Nothing

        True ->
            let
                titles =
                    List.drop 1 childDocumentList |> List.map .title

                levels_ =
                    currentDoc.childLevels

                levels =
                    case List.length levels_ == List.length titles of
                        True ->
                            levels_

                        False ->
                            List.repeat (List.length titles) 0
            in
            Just (List.map2 (\title level -> String.repeat (3 * level) " " ++ title) titles levels |> String.join "\n")


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



-- EDITOR


editingDisplay : ViewInfo -> Model -> Element Msg
editingDisplay viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "FOOTER"

        rt : RenderedText Msg
        rt =
            model.renderedText
    in
    column []
        [ editingHeader viewInfo model rt
        , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, editor viewInfo model, Element.Lazy.lazy (renderedSource viewInfo model footerText) rt ]
        , footer model
        ]


readingDisplay : ViewInfo -> Model -> Element Msg
readingDisplay viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument
                |> Maybe.withDefault "FOOTER"

        rt : RenderedText Msg
        rt =
            model.renderedText
    in
    column [ paddingXY 0 0 ]
        [ readingHeader viewInfo model rt
        , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, Element.Lazy.lazy (renderedSource viewInfo model footerText) rt ]
        , footer model
        ]


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
    in
    row [ spacing 10 ]
        [ column [ width (px w_), height (px h_), clipX, Font.size 12 ]
            [ column [ width (px w2_), paddingXY 10 20 ]
                [ rt.title |> Element.html
                , rt.document |> Element.html
                ]
            ]
        , Element.column [ height (px hToc), width (px wToc), Font.size 12, paddingXY 8 0, Background.color (Style.makeGrey 0.9) ]
            [ column [ height (px (hToc - 125)), scrollbarY, clipX ] [ rt.toc |> Element.html ]
            , column [ paddingXY 12 3, width fill, height (px 125), clipX, Background.color (Style.makeGrey 0.5), Font.color (Style.makeGrey 1.0) ] [ renderFooter footerText_ ]
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



-- DOCUMENT VIEWS (EDITOR, RENDERED, TOC) --
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
            , Input.multiline (Style.textInputStyle w_ h_)
                { onChange = UpdateDocumentText
                , text = Document.getContent model.currentDocument
                , placeholder = Nothing
                , label = Input.labelBelow [ Font.size 0, Font.bold ] (Element.text "")
                , spellcheck = False
                }
            )
        ]


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
    showIf (model.currentUser /= Nothing)
        (Input.button []
            { onPress = Just (SetAppMode (Editing SubdocumentEditing))
            , label =
                el (headerButtonStyle color)
                    (el headerLabelStyle (Element.text "Edit/S"))
            }
        )


newSubdocumentButton model =
    let
        numberOfChildren =
            Maybe.map (.children >> List.length) model.currentDocument
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
            Maybe.map (.children >> List.length) model.currentDocument
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



--)
--
--addSubdocumentButton model =
--    showIf (model.currentUser /= Nothing)
--        (Input.button
--            []
--            { onPress = Just AddSubdocument
--            , label =
--                el []
--                    (el (headingButtonStyle 140) (Element.text "Add subdocument"))
--            }
--        )


inputDocumentId model =
    showIf (model.currentUser /= Nothing)
        (Input.text (Style.inputStyle 140 ++ [ Font.size 11 ])
            { onChange = GotChildDocIdString
            , text = model.childDocIdString
            , placeholder = Nothing
            , label = Input.labelRight [ Font.size 12, Font.color Style.white, width (px 0) ] (Element.text "")
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



-- TOOL PANEL --


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

                DocumentChildren ->
                    case model.tocTree of
                        Nothing ->
                            renderTocForMaster model

                        Just tocTree_ ->
                            renderTocForMaster2 model tocTree_
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


renderTocForSearchResults : Model -> List (Element Msg)
renderTocForSearchResults model =
    List.map (tocEntry model.currentDocument) model.documentList


renderTocForMaster : Model -> List (Element Msg)
renderTocForMaster model =
    let
        master =
            List.head model.childDocumentList |> Maybe.withDefault Data.loadingPage

        list =
            Document.sortChildren master model.childDocumentList

        levels =
            List.map2 (\x y -> ( x, y )) (List.map .title (List.drop 1 list)) master.childLevels

        levelOfTitle : String -> String
        levelOfTitle title =
            List.filter (\( t, _ ) -> title == t) levels
                |> List.head
                |> Maybe.withDefault ( "dummy", 0 )
                |> Tuple.second
                |> (\n -> String.repeat (3 * n) " ")
    in
    List.map (tocEntryForMaster levelOfTitle model.currentDocument) list


renderTocForMaster2 : Model -> Tree TocItem -> List (Element Msg)
renderTocForMaster2 model tocTree_ =
    let
        master =
            List.head model.childDocumentList |> Maybe.withDefault Data.loadingPage

        ( currentDocId, level ) =
            case model.currentDocument of
                Nothing ->
                    ( Utility.getId 0, -2 )

                Just doc ->
                    ( doc.id, Document.getLevel master doc )

        documentList =
            Document.sortChildren master model.childDocumentList

        currentActiveNodeId =
            Maybe.map .id model.currentTocLabel |> Maybe.withDefault (Utility.getId 0)

        tocTree =
            -- case level < 1 of
            case level < 1 of
                True ->
                    tocTree_ |> Toc.setVisibility True currentDocId

                False ->
                    tocTree_ |> Toc.setVisibility True currentActiveNodeId

        renderedToc =
            Toc.render tocTree
    in
    List.map2 (tocEntryForMaster2 model.currentDocument) renderedToc documentList



-- TABLE OF CONTENTS


tocEntryForMaster2 : Maybe Document -> TocItem -> Document -> Element Msg
tocEntryForMaster2 currentDocument_ tocItem document =
    let
        ( color, fontWeight ) =
            tocEntryStyle2 currentDocument_ tocItem

        prefix =
            case tocItem.isRoot of
                True ->
                    "  "

                False ->
                    case tocItem.hasChildren of
                        Just True ->
                            String.repeat ((3 * tocItem.level) - 1) " " ++ "+ "

                        Just False ->
                            "  " ++ String.repeat (3 * tocItem.level) " "

                        Nothing ->
                            "  " ++ String.repeat (3 * tocItem.level) " "
    in
    showIf tocItem.visible
        (Input.button [] { onPress = Just (SetCurrentSubDocument document tocItem), label = el [ Font.color color, fontWeight ] (Element.text (prefix ++ tocItem.title)) })


tocEntryForMaster : (String -> String) -> Maybe Document -> Document -> Element Msg
tocEntryForMaster levelOfTitle currentDocument_ document =
    let
        ( color, fontWeight ) =
            tocEntryStyle currentDocument_ document
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [ Font.color color, fontWeight ] (Element.text (levelOfTitle document.title ++ document.title)) }


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        ( color, fontWeight ) =
            tocEntryStyle currentDocument_ document
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [ Font.color color, fontWeight ] (Element.text document.title) }


tocEntryStyle2 : Maybe Document -> TocItem -> ( Color, Element.Attribute msg )
tocEntryStyle2 currentDocument_ tocItem =
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
                    case ( tocItem.isRoot, currentDocId == tocItem.id ) of
                        ( True, _ ) ->
                            Style.darkBlue

                        ( False, True ) ->
                            Style.darkRed

                        ( False, False ) ->
                            Style.charcoal

        fontWeight =
            case currentDocId == tocItem.id of
                True ->
                    Font.bold

                False ->
                    Font.regular
    in
    ( color, fontWeight )


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
                    case ( currentDocId == document.id, document.children /= [] ) of
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
                    List.length model.childDocumentList

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

        Just _ ->
            case model.documentListType of
                SearchResults ->
                    Input.button []
                        { onPress = Just (SetDocumentListType DocumentChildren)
                        , label =
                            el (headingButtonStyle w)
                                (Element.text ("Documents (" ++ n ++ ")"))
                        }

                DocumentChildren ->
                    Input.button []
                        { onPress = Just (SetDocumentListType SearchResults)
                        , label =
                            el (headingButtonStyle w)
                                (Element.text ("Contents (" ++ n ++ ")"))
                        }



--  el [ Font.size 16, Font.bold ] (Element.text ("Documents" ++ n))
-- HEADER AND FOOTER --
--
--type alias ViewInfo =
--    { toolStripWidth : Float
--    , docListWidth : Float
--    , editorWidth : Float
--    , renderedDisplayWidth : Float
--    , tocWidth : Float
--    , vInset : Float
--    , hExtra : Float
--    }


readingHeader : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
readingHeader viewInfo model rt =
    let
        lhWidth =
            scale (viewInfo.toolStripWidth + viewInfo.docListWidth) model.windowWidth

        -- scale viewInfo.docListWidth model.windowWidth
        titleWidth =
            scale (1.0 * viewInfo.renderedDisplayWidth) model.windowWidth

        rhWidth =
            scale (viewInfo.renderedDisplayWidth + viewInfo.tocWidth) model.windowWidth
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
            [ titleRow titleWidth rt
            , searchRow model
            , el [ width (px 20) ] (Element.text "")
            ]
        ]


searchRow model =
    row [ spacing 10, alignRight ] [ inputSearchTerms model, clearSearchTermsButton, searchButton model, allDocumentsButton, helpDocsButton ]


titleRow titleWidth rt =
    row [ Font.size 12, height (px 40), width (px titleWidth), Font.color Style.white, alignLeft, moveLeft 30 ] [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ]


editTools : Model -> Element Msg
editTools model =
    if List.member model.appMode [ Editing StandardEditing, Editing SubdocumentEditing ] then
        row [ spacing 6 ]
            [ editingModeButton model
            , subDocumentEditingModeButton model
            , newDocumentButton model
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
        , wordCount model
        , el [] (Element.text <| slugOfCurrentDocument model)
        , dirtyDocumentDisplay model
        , displayCurrentTocItem model
        , el [ alignRight, paddingXY 10 0 ] (Element.text <| (model.message |> Tuple.second))
        , currentTime model
        ]


displayCurrentTocItem model =
    case model.currentTocLabel of
        Nothing ->
            el [] (Element.text "-- tocItem --")

        Just tocItem ->
            el [] (Element.text <| "ti: " ++ tocItem.title)


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
