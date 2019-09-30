module Main exposing (main)

import Browser
import Browser.Events
import Data
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Html exposing (..)
import Markdown.Elm
import Markdown.Option exposing (Option(..))
import Prng.Uuid as Uuid
import Random
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import RemoteData exposing (RemoteData(..))
import Request exposing (GraphQLResponse(..), RequestMsg(..))
import Style
import Task
import Time
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


hasuraToken : String
hasuraToken =
    "GOc97wA7CCMm31H4UJHa-4pqdVoLf3l6gAwzczdHC"


type alias Message =
    ( MessageType, String )


type MessageType
    = SignInMessage
    | UserMessage
    | ErrorMessage
    | DebugMessage


type alias Model =
    { seed : Int

    -- UI
    , option : Option
    , windowWidth : Int
    , windowHeight : Int
    , visibilityOfTools : Visibility
    , appMode : AppMode
    , message : Message

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
    , currentDocument : Maybe Document
    , currentDocumentDirty : Bool
    , secondsWhileDirty : Int
    , tagString : String
    , searchTerms : String
    }


type AppMode
    = Reading
    | Editing
    | UserMode UserState


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

        Editing ->
            "Editing"

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

        model =
            { seed = 0

            -- UI
            , option = ExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , visibilityOfTools = Invisible
            , appMode = UserMode SignInState
            , message = ( UserMessage, "Starting ..." )

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
            , currentDocument = Nothing
            , currentDocumentDirty = False
            , secondsWhileDirty = 0
            , tagString = ""
            , searchTerms = ""
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
    | SelectStandard
    | SelectExtended
    | SelectExtendedMath
    | SetToolPanelState Visibility
    | SetAppMode AppMode
    | WindowSize Int Int
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
    | GetPublicDocuments
    | ArmForDelete
    | DeleteDocument
    | CancelDeleteDocument
    | UpdateDocumentText String
    | GotSearchTerms String
    | DoSearch
    | SetCurrentDocument Document
    | SetDocumentPublic Bool
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

        SelectStandard ->
            ( { model
                | option = Standard
              }
            , Cmd.none
            )

        SelectExtended ->
            ( { model
                | option = Extended
              }
            , Cmd.none
            )

        SelectExtendedMath ->
            ( { model
                | option = ExtendedMath
              }
            , Cmd.none
            )

        SetToolPanelState visibility ->
            ( { model | visibilityOfTools = visibility }, Cmd.none )

        SetAppMode appMode ->
            case appMode of
                Reading ->
                    ( { model | appMode = Reading, visibilityOfTools = Invisible }, Cmd.none )

                Editing ->
                    ( { model | appMode = Editing, visibilityOfTools = Visible }, Cmd.none )

                UserMode s ->
                    case model.currentUser of
                        Nothing ->
                            ( { model | appMode = UserMode SignInState }, Cmd.none )

                        Just _ ->
                            ( { model | appMode = UserMode s }, Cmd.none )

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

        WindowSize width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

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
            if (model.username == "jxxcarlson" || model.username == "boris") && model.password == "locoLobo" then
                ( { model
                    | currentUser = Just (User.dummy model.username)
                    , appMode = Reading
                    , visibilityOfTools = Invisible
                  }
                , Request.documentsByAuthor hasuraToken model.username |> Cmd.map Req
                )

            else
                ( { model | currentUser = Nothing, appMode = UserMode SignInState, password = "" }, Cmd.none )

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
            case model.currentUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        newDocument =
                            Document.create model.currentUuid user.username "New Document" "# New Document\n\nWrite something here ..."

                        ( newUuid, newSeed ) =
                            step Uuid.generator model.currentSeed
                    in
                    ( { model
                        | currentDocument = Just newDocument
                        , documentList = newDocument :: model.documentList
                        , visibilityOfTools = Invisible
                        , currentUuid = newUuid
                        , currentSeed = newSeed
                      }
                    , Request.insertDocument hasuraToken newDocument |> Cmd.map Req
                    )

        SaveDocument ->
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

        ArmForDelete ->
            ( { model | documentDeleteState = Armed, message = ( UserMessage, "Armed for delete.  Caution!" ) }, Cmd.none )

        CancelDeleteDocument ->
            ( { model | documentDeleteState = SafetyOn, message = ( UserMessage, "Delete cancelled" ) }, Cmd.none )

        DeleteDocument ->
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

        GetUserDocuments ->
            case model.currentUser of
                Nothing ->
                    ( { model | message = ( UserMessage, "Can't get documents if user is not signed in" ) }, Cmd.none )

                Just user ->
                    ( { model
                        | message = ( UserMessage, "Getting your documents" )
                        , visibilityOfTools = Invisible
                      }
                    , Request.documentsByAuthor hasuraToken user.username |> Cmd.map Req
                    )

        GetPublicDocuments ->
            ( model, Request.publicDocuments hasuraToken |> Cmd.map Req )

        UpdateDocumentText str ->
            case model.currentDocument of
                Nothing ->
                    ( model, Cmd.none )

                Just doc ->
                    let
                        updatedDoc1 =
                            Document.setContent str doc

                        updatedDoc2 =
                            Document.updateMetaData updatedDoc1
                    in
                    ( { model
                        | currentDocument = Just updatedDoc2
                        , documentList = Document.replaceInList updatedDoc2 model.documentList
                        , currentDocumentDirty = True
                      }
                    , Cmd.none
                    )

        SetCurrentDocument document ->
            ( { model
                | currentDocument = Just document
                , tagString = document.tags |> String.join ", "
                , counter = model.counter + 1
              }
            , Cmd.none
            )

        SetDocumentPublic bit ->
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

        GotTagString str ->
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

        GotSearchTerms str ->
            ( { model | searchTerms = str }, Cmd.none )

        DoSearch ->
            ( model, Request.documentsByTitle hasuraToken model.searchTerms |> Cmd.map Req )

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
                            let
                                currentDoc =
                                    List.head documentList
                            in
                            ( { model
                                | documentList = documentList
                                , currentDocument = currentDoc
                                , tagString = getTagString currentDoc
                                , message = ( UserMessage, "Success getting document list" )
                              }
                            , Cmd.none
                            )

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
                                    Document.getById "e6880153-cc25-48b6-9d71-aae6653aad23" documentList
                            in
                            ( { model
                                | documentList = documentList
                                , currentDocument = currentDoc
                                , tagString = getTagString currentDoc
                                , message = ( UserMessage, "Success getting document list" )
                              }
                            , Cmd.none
                            )

                InsertDocumentResponse _ ->
                    ( { model | message = ( UserMessage, "New document saved" ) }, Cmd.none )



-- UPDATE HELPERS --


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
            in
            ( { model
                | documentList = newDocumentList
                , currentDocument = List.head newDocumentList
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

        Editing ->
            Element.layoutWith { options = [ focusStyle myFocusStyle ] } [] (editingDisplay viewInfoEditing model)

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
    { lhsFraction = 0.7
    , rhsFraction = 0.3
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
        w =
            scale viewInfo.rhsFraction model.windowWidth

        h =
            translate -viewInfo.vInset model.windowHeight

        rt =
            Markdown.Elm.toHtml ExtendedMath Data.rhsUserText
    in
    column [ width (px w), height (px h), padding 36, scrollbarY, Background.color (Style.makeGrey 0.9), Font.color (Style.makeGrey 0.1) ]
        [ rt |> Element.html ]


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
        [ modeButtonStrip model
        , column [ width (px lhsWidth), Font.size 12, Font.color Style.white, alignRight, moveUp 8 ] []
        , column [ width (px rhsWidth) ] []
        ]


modeButtonStrip model =
    row [ width fill, height (px 45), spacing 10, paddingXY 20 0 ]
        [ editTools model
        , readingModeButton model
        , userPageModeButton model
        ]



-- SIGN-IN


signInUpView model =
    column Style.signInColumn
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (Element.text "Welcome!")
        , column [ spacing 8, paddingXY 0 18 ]
            [ el [ Font.size 14 ] (Element.text "Explore public documents without signing in — Click on 'Read', above left.")
            , el [ Font.size 14 ] (Element.text "Edits are saved only for your documents and only if you are signed in")
            , el [ Font.size 14 ] (Element.text "User sign in/sign up coming soon.")
            , el [ Font.size 14 ] (Element.text "")
            , el [ Font.size 14 ] (Element.text "This project is a work-in-progress.")
            , el [ Font.size 14 ] (Element.text "Comments to jxxcarlson on the Elm Slack or to jxxcarlson at gmail")
            ]
        , outerPasswordPanel model
        ]


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


passwordPanel model =
    column [ spacing 12, paddingXY 0 18 ]
        [ inputCurrentPassword model
        , inputNewPassword1 model
        , inputNewPassword2 model
        , el [ Font.size 16, Font.color Style.darkRed ] (Element.text (signInMessage model.message))
        ]


inputCurrentPassword model =
    Input.currentPassword (Style.inputStyle 200)
        { onChange = GotPassword
        , text = model.password
        , placeholder = Nothing
        , show = False

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (Element.text "Old password: ")
        }


inputNewPassword1 model =
    Input.newPassword (Style.inputStyle 200)
        { onChange = GotNewPassword1
        , show = False
        , text = model.newPassword1
        , placeholder = Nothing

        ---, show = False
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 110) ] (Element.text "New password: ")
        }


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
    Input.button Style.headerButton
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


inputUserName model =
    Input.text (Style.inputStyle 200)
        { onChange = GotUserName
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Username")
        }


inputEmail model =
    Input.text (Style.inputStyle 200)
        { onChange = GotEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, moveDown 8, width (px 100) ] (Element.text "Email")
        }


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
    Input.button Style.headerButton
        { onPress = Just SignIn
        , label = Element.text "Sign in"
        }


signUpButton : Model -> Element Msg
signUpButton model =
    Input.button Style.headerButton
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
    Input.button Style.headerButton
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
    Input.button Style.headerButton
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
    Input.button Style.headerButton
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



-- EDITOR --


editingDisplay : ViewInfo -> Model -> Element Msg
editingDisplay viewInfo model =
    let
        rt : { title : Html msg, toc : Html msg, document : Html msg }
        rt =
            Markdown.Elm.toHtmlWithExternaTOC model.option (Document.getContent model.currentDocument)
    in
    column []
        [ header viewInfo model rt
        , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, editor viewInfo model, renderedSource viewInfo model rt ]
        , footer model
        ]


readingDisplay : ViewInfo -> Model -> Element Msg
readingDisplay viewInfo model =
    let
        footerText =
            Maybe.map Document.footer model.currentDocument |> Maybe.withDefault ""

        rt : { title : Html msg, toc : Html msg, document : Html msg }
        rt =
            Markdown.Elm.toHtmlWithExternaTOC model.option (Document.getContent model.currentDocument ++ footerText)
    in
    column [ paddingXY 0 0 ]
        [ header viewInfo model rt
        , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, renderedSource viewInfo model rt ]
        , footer model
        ]


toolsOrDocs viewInfo model =
    case ( model.visibilityOfTools, model.appMode ) of
        ( Visible, Editing ) ->
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
            ( String.fromInt model.counter
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
            if model.appMode == Editing then
                Style.red

            else
                Style.buttonGrey
    in
    Input.button []
        { onPress = Just (SetAppMode Editing)
        , label =
            el (headerButtonStyle color)
                (el headerLabelStyle (Element.text "Edit"))
        }


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



-- RENDERED SOURCE --


renderedSource : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element msg
renderedSource viewInfo model rt =
    let
        token =
            String.fromInt model.counter

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
        [ Element.Keyed.column [ width (px w_), height (px h_), clipX, Font.size 12 ]
            [ ( token, column [ width (px w2_), paddingXY 10 20 ] [ rt.document |> Element.html ] ) ]
        , Element.column [ width (px wToc), height (px hToc), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (Style.makeGrey 0.9) ]
            [ rt.toc |> Element.html ]
        ]



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
        , label = Input.labelAbove [ Font.size 12, Font.bold, Font.color Style.white ] (Element.text "Tags")
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
                        , label = el toolButtonStyleInHeader (Element.text "Public")
                        }

                False ->
                    Input.button []
                        { onPress = Just (SetDocumentPublic True)
                        , label = el toolButtonStyleInHeader (Element.text "Private")
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
    in
    column
        [ width (px (scale viewInfo.docListWidth model.windowWidth))
        , height (px h_)
        , Background.color (Style.makeGrey 0.9)
        , paddingXY 12 20
        , alignTop
        , clipX
        ]
        [ column [ Font.size 13, spacing 8 ] (heading model :: List.map (tocEntry model.currentDocument) model.documentList) ]


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        color =
            case currentDocument_ of
                Nothing ->
                    Style.buttonGrey

                Just currentDocument ->
                    if currentDocument.id == document.id then
                        Style.red

                    else
                        Style.buttonGrey
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [ Font.color color ] (Element.text document.title) }


heading : Model -> Element Msg
heading model =
    case model.currentUser of
        Nothing ->
            el [ Font.size 16, Font.bold ] (Element.text "Public documents")

        Just _ ->
            el [ Font.size 16, Font.bold ] (Element.text "Documents")



-- HEADER AND FOOTER --


header : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
header viewInfo model rt =
    let
        renderedDisplayWidth_ =
            scale viewInfo.renderedDisplayWidth model.windowWidth

        innerTOCWidth_ =
            scale viewInfo.tocWidth model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ modeButtonStrip model
        , column [ width (px renderedDisplayWidth_), Font.size 12, Font.color Style.white, alignRight, moveUp 8 ] [ rt.title |> Element.html |> Element.map (\_ -> NoOp) ]
        , column [ width (px innerTOCWidth_) ]
            [ row [ spacing 10 ]
                [ inputSearchTerms model
                , searchButton
                , allUserDocumentsButton
                , publicDocumentsButton
                ]
            ]
        ]


inputSearchTerms model =
    Input.text (Style.inputStyle 200 ++ [ alignLeft ])
        { onChange = GotSearchTerms
        , text = model.searchTerms
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, width (px 0) ] (Element.text "")
        }


searchButton : Element Msg
searchButton =
    Input.button []
        { onPress = Just DoSearch
        , label =
            el [ height (px 30), width (px 30), centerX, padding 8, Background.color Style.blue, Font.color Style.white, Font.size 12 ]
                (Element.text "S")
        }


allUserDocumentsButton : Element Msg
allUserDocumentsButton =
    Input.button []
        { onPress = Just GetUserDocuments
        , label =
            el [ height (px 30), width (px 30), centerX, padding 8, Background.color Style.blue, Font.color Style.white, Font.size 12 ]
                (Element.text "M")
        }


publicDocumentsButton : Element Msg
publicDocumentsButton =
    Input.button []
        { onPress = Just GetPublicDocuments
        , label =
            el [ height (px 30), width (px 30), centerX, padding 8, Background.color Style.blue, Font.color Style.white, Font.size 12 ]
                (Element.text "P")
        }


editTools : Model -> Element Msg
editTools model =
    if model.appMode == Editing then
        row [ spacing 6 ]
            [ editingModeButton model
            , newDocumentButton model
            , saveDocumentButton model
            , deleteDocumentButton model
            , cancelDeleteDocumentButtonInHeader model
            ]

    else
        editingModeButton model



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
        Editing ->
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
        , el [ alignRight, paddingXY 10 0 ] (Element.text <| (model.message |> Tuple.second))
        , currentTime model
        ]


dirtyDocumentDisplay : Model -> Element Msg
dirtyDocumentDisplay model =
    case ( model.appMode == Editing, model.currentUser ) of
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
    column [ spacing 10, Background.color Style.charcoal, padding 22 ]
        [ el [ Font.color Style.white, Font.bold ] (Element.text "Markdown Flavor")
        , standardMarkdownButton model 93
        , extendedMarkdownButton model 93
        , extendedMathMarkdownButton model 93
        ]



---- BUTTONS --


standardMarkdownButton model width =
    let
        bit =
            model.option == Standard
    in
    Input.button (Style.buttonStyleSelected width bit)
        { onPress = Just SelectStandard, label = el [ centerX ] (Element.text "Standard") }


extendedMarkdownButton model width =
    let
        bit =
            model.option == Extended
    in
    Input.button (Style.buttonStyleSelected width bit)
        { onPress = Just SelectExtended, label = el [ centerX ] (Element.text "Extended") }


extendedMathMarkdownButton model width =
    let
        bit =
            model.option == ExtendedMath
    in
    Input.button (Style.buttonStyleSelected width bit)
        { onPress = Just SelectExtendedMath, label = el [ centerX ] (Element.text "ExtendedMath") }
