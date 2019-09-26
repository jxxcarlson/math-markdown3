module Main exposing (main)

import Parse
import Browser
import Browser.Events
import Html exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Markdown.Elm
import Html.Attributes
import Markdown.Option exposing (Option(..))
import Random
import Data
import Document exposing(Document)
import Time
import Task
import Utility exposing (humanTimeHM)
import User exposing(User)
import Request exposing(RequestMsg(..))
import RemoteData exposing (RemoteData(..))
import Style
import Api.Scalar exposing(Id(..))

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
hasuraToken = "GOc97wA7CCMm31H4UJHa-4pqdVoLf3l6gAwzczdHC"

type alias Model =
    {
      seed : Int
    -- UI
    , option : Option
    , windowWidth : Int
    , windowHeight : Int
    , visibilityOfTools : Visibility
    , appMode: AppMode
    , message : String
    -- TIME
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
    }

type AppMode = Reading | Editing | UserMode UserState

type UserState
    = SignInState
    | SignUpState
    | ChangePasswordState
    | SignedInState



appStateAsString : Model -> String
appStateAsString model =
    case model.appMode of
        Reading -> "Reading"
        Editing -> "Editing"
        UserMode SignInState -> "Signing in"
        UserMode SignUpState -> "Signing up"
        UserMode ChangePasswordState -> "Changing Password"
        UserMode SignedInState -> "Signed in"


type DocumentDeleteState = SafetyOn | Armed

type Visibility = Visible | Invisible



-- INIT --

init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            {
             seed = 0
             -- UI
            , option = ExtendedMath
            , windowWidth = flags.width
            , windowHeight = flags.height
            , visibilityOfTools = Invisible
            , appMode = UserMode SignInState
            , message = "Starting ..."

            -- TIME
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
            , documentList = [Data.startupDocument, Data.doc5, Data.doc2, Data.doc3, Data.doc4]
            , currentDocument = Just Data.startupDocument
            }
    in
    ( model, Task.perform AdjustTimeZone Time.here )

-- MSG --

type Msg
    = NoOp
    -- Time
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
    | ArmForDelete
    | DeleteDocument
    | CancelDeleteDocument
    | UpdateDocumentText String
    | SetCurrentDocument Document
    | Clear
    | Req RequestMsg






type alias Flags =
    {  width : Int
      , height : Int
    }




type alias ViewInfo = {
      toolStripWidth : Float
    , docListWidth : Float
    , editorWidth : Float
    , renderedDisplayWidth : Float
    , tocWidth : Float
    , vInset : Float
    , hExtra: Float

  }

vInset = 75 -- 208

viewInfoEditing = {
    toolStripWidth = 0.05
  , docListWidth = 0.15
  , editorWidth = 0.3
  , renderedDisplayWidth = 0.3
  , tocWidth = 0.2
  , vInset = vInset
  , hExtra = 0
  }

viewInfoReading = {
    toolStripWidth = 0.05
  ,  docListWidth = 0.25
  , editorWidth = 0
  , renderedDisplayWidth = 0.45
  , tocWidth = 0.25
  , vInset = vInset
  , hExtra = 0
  }

scale : Float -> Int -> Int
scale factor input =
    factor * (toFloat input) |> round

affine : Float -> Float -> Int -> Int
affine factor shift input =
    factor * ((toFloat input) - shift) |> round

translate : Float -> Int -> Int
translate amount input =
    (toFloat input)  + amount |> round

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Time.every 1000 Tick
        , Browser.Events.onResize WindowSize
      ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        GenerateSeed ->
            ( model, Random.generate NewSeed (Random.int 1 10000) )

        NewSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        Clear ->
            ( { model
                |  counter = model.counter + 1
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
            ( {model | visibilityOfTools = visibility}, Cmd.none)


        SetAppMode appMode ->
            case appMode of
                Reading ->  ( {model | appMode = Reading}, Cmd.none)
                Editing ->  ( {model | appMode =  Editing}, Cmd.none)
                UserMode s ->  ( {model | appMode =  UserMode s}, Cmd.none)


        -- TIME --

        Tick newTime ->
              ( { model | time = newTime }
              , Cmd.none
              )

        WindowSize width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        AdjustTimeZone newZone ->
              ( { model | zone = newZone }
              , Cmd.none
              )

        -- USER --

        GotUserName str -> ({ model | username = str} , Cmd.none)

        GotPassword str -> ({ model | password = str}, Cmd.none)

        GotNewPassword1 _ -> (model, Cmd.none)
        GotNewPassword2 _ -> (model, Cmd.none)
        ChangePassword -> (model, Cmd.none)
        GotEmail _ -> (model, Cmd.none)

        SignIn ->
          if model.username == "jxxcarlson" && model.password == "locoLobo" then
            ({ model | currentUser = Just User.dummy, appMode = Reading }, Cmd.none)
           else
            ({model | currentUser = Nothing, appMode = UserMode SignInState, password = ""} , Cmd.none)



        SignUp -> ({model | appMode = UserMode SignUpState, message = "At SignUp msg"} , Cmd.none)


        SignOut -> ({model | currentUser = Nothing, appMode = UserMode SignInState} , Cmd.none)

        -- DOCUMENT --

        CreateDocument ->
            (model, Cmd.none)
--           case model.currentUser of
--               Nothing -> (model, Cmd.none)
--               Just user ->
--                   let
--                      newDocument = Document.create user.username "New Document" model.time "# New Document\n\nWrite something here ..."
--                   in
--                   ({model | currentDocument = Just newDocument
--                            , documentList = newDocument :: model.documentList
--                            , visibilityOfTools = Invisible
--                     }
--                     , Request.createDocument newDocument |> Cmd.map Req
--                   )


        SaveDocument ->
            (model, Cmd.none)
--            case model.currentDocument of
--               Nothing -> (model, Cmd.none)
--               Just document ->
--                   ({model | message = "Saving document ..."}
--                     , Request.update_document document |> Cmd.map Req
--                   )

        ArmForDelete ->
            ({ model | documentDeleteState = Armed, message = "Armed for delete.  Caution!"}, Cmd.none)

        CancelDeleteDocument ->
            ({ model | documentDeleteState = SafetyOn, message = "Delete cancelled"}, Cmd.none)

        DeleteDocument ->
            (model, Cmd.none)
--            case model.currentDocument of
--               Nothing -> (model, Cmd.none)
--               Just document ->
--                   case model.documentDeleteState of
--                       SafetyOn ->
--                           ({model | message = "Turning safety off.  Press again to delete document.", documentDeleteState = Armed}
--                             , Cmd.none)
--                       Armed ->
--                         ({model | message = "Deleting document ...", documentDeleteState = SafetyOn}
--                             , Request.delete_document document |> Cmd.map Req)

        GetUserDocuments ->
            case model.currentUser of
                Nothing -> ({model | message = "Can't get documents if user is not signed in"}, Cmd.none)
                Just user ->
                    ({model | message = "Getting your documents"
                       , visibilityOfTools = Invisible }, Request.documentsByAuthor hasuraToken user.username |> Cmd.map Req)

        UpdateDocumentText str ->
            case model.currentDocument of
                Nothing -> (model, Cmd.none)
                Just doc ->
                   let
                     updatedDoc = Document.setContent str doc
                   in
                     ( { model | currentDocument = Just updatedDoc
                               , documentList = Document.replaceInList updatedDoc model.documentList
                       }
                       ,
                       Cmd.none
                     )

        SetCurrentDocument document ->
            ( {model | currentDocument = Just document, counter = model.counter + 1}, Cmd.none)


        Req requestMsg ->
            case requestMsg of
--              GotNewDocument remoteData ->
--                case remoteData of
--                   NotAsked -> ({ model | message = "New doc: not asked"} , Cmd.none)
--                   Loading -> ({model | message = "Mew doc: loading"} , Cmd.none)
--                   Failure _ ->
--                        ({model | message = "New doc: failed request"} , Cmd.none)
--                   Success _ -> ({model | message = "New document created"} , Cmd.none)
--
--              ConfirmUpdatedDocument remoteData ->
--                  case remoteData of
--                     NotAsked -> ({ model | message = "Update doc: not asked"} , Cmd.none)
--                     Loading -> ({model | message = "Updated doc: loading"} , Cmd.none)
--                     Failure _ ->
--                          ({model | message = "Updated doc: failed request"} , Cmd.none)
--                     Success _ -> ({model | message = "Document update successful"} , Cmd.none)
--
--              ConfirmUDeleteDocument remoteData ->
--                  case remoteData of
--                     NotAsked -> ({ model | message = "Delete doc: not asked"} , Cmd.none)
--                     Loading -> ({model | message = "Delete doc: loading"} , Cmd.none)
--                     Failure _ ->
--                          ({model | message = "Delete doc: failed request"} , Cmd.none)
--                     Success maybeDeletedDocument -> handleDeletedDocument model maybeDeletedDocument
--                       -- ({model | message = "Document delete successful"} , Cmd.none)

              GotUserDocuments remoteData  ->
                case remoteData of
                   NotAsked -> ({ model | message = "Get author docs: not asked"} , Cmd.none)
                   Loading -> ({model | message = "Get author docs:: loading"} , Cmd.none)
                   Failure _ ->
                       ({model | message = "Get author docs:: request failed"} , Cmd.none)
                   Success documentList ->
                         ({model | documentList = documentList
                                 , currentDocument = List.head documentList
                                 , message = "Success returning document list for author!"} , Cmd.none)
              InsertDocumentResponse _ ->
                  ({model | message = "Maybe new document saved"}, Cmd.none)



          -- MANAGE DOCUMENTS --

handleDeletedDocument model maybeDeletedDocument =
    case maybeDeletedDocument  of
        Nothing -> ({model | message = "Odd, something weird happened in deleting your document"}, Cmd.none)
        Just deletedDocument ->
            let
               newDocumentList = List.filter (\doc -> doc.id /= deletedDocument.id )  model.documentList
            in
               ({ model | documentList = newDocumentList
                  , currentDocument = List.head newDocumentList
                  , message = "Document " ++ deletedDocument.title ++ " deleted"
                  , visibilityOfTools = Invisible}
                , Cmd.none)

--
-- VIEW FUNCTIONS
---

type alias RenderedDocumentRecord msg = { document : Html msg, title : Html msg, toc : Html msg }

view : Model -> Html Msg
view model =
    case model.appMode of
        Reading ->  Element.layoutWith {options = [focusStyle myFocusStyle] }  []  (readingDisplay viewInfoReading model)
        Editing ->  Element.layoutWith {options = [focusStyle myFocusStyle] }  []  (editingDisplay viewInfoEditing model)
        UserMode _ ->  Element.layoutWith {options = [focusStyle myFocusStyle] }  []  (userPageDisplay viewInfoUserPage model)


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

viewInfoUserPage = {
    lhsFraction = 0.7
  , rhsFraction = 0.3
  , vInset = vInset
  }


userPageDisplay : ViewInfoUserPage -> Model -> Element Msg
userPageDisplay viewInfo model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
  in
    column [ Background.color (makeGrey 0.35), Font.color white]
           [
              userPageHeader viewInfo model
            , row [height (px h_), Font.size 14] [ lhsViewInfoPage viewInfo model, rhsViewInfoPage viewInfo model ]
            , userPageFooter model
            ]


lhsViewInfoPage viewInfo model =
    let
          w = scale viewInfo.lhsFraction model.windowWidth
          h = translate (-viewInfo.vInset) model.windowHeight
       in
        column [width (px w), height (px h), padding 12, spacing 24 ] [

          if model.appMode == UserMode SignInState then
             signInUpView model
          else
             case model.currentUser of
                 Nothing -> signInUpView model
                 Just user ->  signedInUserView model user
           ]


rhsViewInfoPage viewInfo model =
   let
      w = scale viewInfo.rhsFraction model.windowWidth
      h = translate (-viewInfo.vInset) model.windowHeight
      rt = Markdown.Elm.toHtml ExtendedMath Data.rhsUserText
   in
    column [width (px w), height (px h), padding 36, scrollbarY, Background.color (makeGrey 0.8), Font.color (makeGrey 0.1) ]
       [rt |> Element.html ]


userPageFooter : Model -> Element Msg
userPageFooter model =
       row [ paddingXY 20 0, height (px 30), width (px (model.windowWidth)), Background.color charcoal, Font.color white, spacing 24, Font.size 12] [
             el [] (Element.text <| appStateAsString model)
           , el [] (Element.text <| model.message)
       ]

userPageHeader : ViewInfoUserPage -> Model -> Element Msg
userPageHeader  viewInfo model =
  let
     lhsWidth =  scale viewInfo.lhsFraction model.windowWidth
     rhsWidth =   scale viewInfo.rhsFraction model.windowWidth
  in
    row [ height (px 45), width (px (model.windowWidth)), Background.color charcoal] [
      modeButtonStrip model
     , column [width (px lhsWidth), Font.size 12, Font.color white, alignRight, moveUp 8] []
     , column [width (px rhsWidth)] []
    ]

modeButtonStrip model =
   row [width (px 400 ), height (px 45), spacing 10, paddingXY 20 0] [ editTools model,  readingModeButton model, userPageModeButton model]


-- SIGN-IN


noUserView model =
    column [] [Element.text "Now one signed in"]


signInUpView model =
    column Style.mainColumnX
        [ el [ Font.size 18, Font.bold, paddingXY 0 12 ] (Element.text "Welcome!")
        , el [ Font.size 14, Font.bold, paddingXY 0 12 ] (Element.text "For now, please sign in as 'demo' with password 'demo'.")
        , inputUserName model
        , inputPassword model
        , showIf (model.appMode == UserMode SignUpState) (inputEmail model)
        , showIf (model.appMode == UserMode SignUpState) (el [ Font.size 12 ] (Element.text "A real email address is only needed for password recovery in real production."))
        , row [ spacing 12, paddingXY 0 12 ]
            [ showIf (model.appMode == UserMode SignInState) (signInButton)
            , row [ spacing 12 ]
                [ signUpButton model
                , showIf (model.appMode == UserMode SignUpState) (cancelSignUpButton model)
                ]
            ]
        , el [ Font.size 16, Font.color Style.white ] (Element.text model.message)
        ]



signedInUserView : Model -> User -> Element Msg
signedInUserView model user =
    column Style.mainColumnX
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
        , el [ Font.size 16, Font.color Style.darkRed ] (Element.text model.message)
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


signInButton :  Element Msg
signInButton  =
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
      rt : {title: Html msg, toc: Html msg, document: Html msg}
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option (Document.getContent model.currentDocument)
  in
    column []
        [
           header viewInfo model rt
         , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, editor viewInfo model, renderedSource viewInfo model rt ]
         , footer model
         ]

readingDisplay : ViewInfo -> Model -> Element Msg
readingDisplay viewInfo model =
  let
      footerText = Maybe.map Document.footer model.currentDocument |> Maybe.withDefault ""
      rt : {title: Html msg, toc: Html msg, document: Html msg}
      rt = Markdown.Elm.toHtmlWithExternaTOC model.option ((Document.getContent model.currentDocument) ++ footerText )
  in
    column [ paddingXY 0 0]
        [
           header viewInfo model rt
         , row [] [ tabStrip viewInfo model, toolsOrDocs viewInfo model, renderedSource viewInfo  model rt ]
         , footer model
         ]

toolsOrDocs viewInfo model =
    case model.visibilityOfTools of
        Visible ->  toolPanel viewInfo model
        Invisible -> docListViewer viewInfo model

-- DOCUMENT VIEWS (EDITOR, RENDERED, TOC) --

-- EDITOR --

editor : ViewInfo -> Model -> Element Msg
editor viewInfo model =
   let
       w_ = affine viewInfo.editorWidth (viewInfo.hExtra) model.windowWidth |> toFloat
       h_ = translate (-viewInfo.vInset) model.windowHeight |> toFloat

   in
    column []
            [ Element.Keyed.el []
                ( String.fromInt model.counter
                , Input.multiline (textInputStyle w_ h_)
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
              UserMode _ -> red
              _ -> buttonGrey

  in
     Input.button [] { onPress = Just (SetAppMode (UserMode SignedInState))
            , label = el (headerButtonStyle color)
            (el (headerLabelStyle) (Element.text "User"))}



editingModeButton model =
  let
      color = if model.appMode == Editing then
           red
        else
           buttonGrey
  in
     Input.button [] { onPress = Just (SetAppMode Editing)
            , label = el (headerButtonStyle color)
            (el (headerLabelStyle) (Element.text "Edit"))}


readingModeButton model =
  let
      color = if model.appMode == Reading then
           red
        else
           buttonGrey
  in
    Input.button [] { onPress = Just (SetAppMode Reading)
            , label = el (headerButtonStyle color)
       (el (headerLabelStyle) (Element.text "Read"))}


headerButtonStyle color = [ height (px 30), width (px 50), Background.color color, Font.color white, Font.size 12]
headerLabelStyle = [height (px 30), width (px 80), padding 8]

-- RENDERED SOURCE --

renderedSource : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element msg
renderedSource viewInfo model rt =
    let
        token =
            String.fromInt model.counter

        w_ = affine viewInfo.renderedDisplayWidth (viewInfo.hExtra) model.windowWidth
        h_ = translate (-viewInfo.vInset) model.windowHeight

        w2_ = affine viewInfo.renderedDisplayWidth (viewInfo.hExtra + 160) model.windowWidth

        wToc = affine viewInfo.tocWidth (viewInfo.hExtra) model.windowWidth
        hToc = translate (-viewInfo.vInset) model.windowHeight
    in
      row [spacing 10] [
         Element.Keyed.column [width (px w_), height (px h_),               clipX, Font.size 12]
           [ ( token, column [width (px w2_), paddingXY 10 20 ] [rt.document |> Element.html] ) ]
        , Element.column [width (px wToc), height (px hToc), scrollbarY, Font.size 12, paddingXY 20 0, Background.color (makeGrey 0.9)]
                                    [ rt.toc |> Element.html  ]
      ]

-- TOOL PANEL --

toolPanel viewInfo model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
      heading_ = el [Font.size 16, Font.bold] (Element.text "Report bugs!  ")
  in
    column [width (px (scale viewInfo.docListWidth model.windowWidth)), height (px h_), Background.color (makeGrey 0.5)
       , paddingXY 20 20, alignTop]
      [column [Font.size 13, spacing 15]  [
          el [Font.size 16, Font.bold, Font.color white] (Element.text "Document tools")
        , getUserDocumentsButton
        , flavors model
       ]
  ]


toolButtonStyle : List (Element.Attribute msg)
toolButtonStyle = [height (px 30), width (px 150),  padding 8, Background.color charcoal, Font.color white, Font.size 12]

toolButtonStyleInHeader : List (Element.Attribute msg)
toolButtonStyleInHeader = [height (px 30), width (px 60),  padding 8, Background.color (makeGrey 0.1), Border.color white, Font.color white, Font.size 12]

newDocumentButton =
        Input.button [] { onPress = Just (CreateDocument)
                , label = el toolButtonStyle (Element.text "Create")}

newDocumentButtonInHeader =
        Input.button [] { onPress = Just (CreateDocument)
                , label = el toolButtonStyleInHeader (Element.text "Create")}

saveDocumentButton =
        Input.button [] { onPress = Just (SaveDocument)
                , label = el toolButtonStyle (Element.text "Save")}

saveDocumentButtonInHeader =
        Input.button [] { onPress = Just (SaveDocument)
                , label = el toolButtonStyleInHeader (Element.text "Save")}

getUserDocumentsButton =
        Input.button [] { onPress = Just (GetUserDocuments)
                , label = el toolButtonStyle (Element.text "Get")}

deleteDocumentButton model =
    case model.documentDeleteState of
       Armed ->
         Input.button [] { onPress = Just (DeleteDocument)
                , label = el (toolButtonStyle ++ [Background.color red]) (Element.text "Delete!")}
       SafetyOn ->
          Input.button [] { onPress = Just ArmForDelete
                          , label = el toolButtonStyle (Element.text "Delete?")}

deleteDocumentButtonInHeader model =
      case model.documentDeleteState of
         Armed ->
           Input.button [] { onPress = Just (DeleteDocument)
                  , label = el (toolButtonStyleInHeader ++ [Background.color red]) (Element.text "Delete!")}
         SafetyOn ->
            Input.button [] { onPress = Just ArmForDelete
                            , label = el toolButtonStyleInHeader (Element.text "Delete?")}

cancelDeleteDocumentButtonInHeader model =
      case model.documentDeleteState of
         Armed ->
           Input.button [] { onPress = Just (CancelDeleteDocument)
                  , label = el (toolButtonStyleInHeader ++ [Background.color blue]) (Element.text "Cancel")}
         SafetyOn ->
            Element.none


-- DOCUMENT LIST --

docListViewer viewInfo model =
  let
      h_ = translate (-viewInfo.vInset) model.windowHeight
  in
    column [width (px (scale viewInfo.docListWidth model.windowWidth)), height (px h_), Background.color (makeGrey 0.9)
       , paddingXY 12 20, alignTop, clipX]
      [column [Font.size 13, spacing 8]  (heading::(List.map (tocEntry model.currentDocument) model.documentList))]


tocEntry : Maybe Document -> Document -> Element Msg
tocEntry currentDocument_ document =
    let
        color = case currentDocument_ of
            Nothing ->  buttonGrey
            Just currentDocument ->
                if currentDocument.identifier == document.identifier then
                   red
                else
                   buttonGrey
    in
    Input.button [] { onPress = Just (SetCurrentDocument document), label = el [Font.color color] (Element.text document.title)}


heading = el [Font.size 16, Font.bold] (Element.text "Documents")


-- HEADER AND FOOTER --

header : ViewInfo -> Model -> RenderedDocumentRecord msg -> Element Msg
header viewInfo model rt =
  let
     editorWidth_ =  scale viewInfo.editorWidth model.windowWidth
     renderedDisplayWidth_ =   scale viewInfo.renderedDisplayWidth model.windowWidth
     innerTOCWidth_ = scale viewInfo.tocWidth model.windowWidth
  in
    row [ height (px 45), width (px (model.windowWidth)), Background.color charcoal] [
      modeButtonStrip model
     , column [width (px renderedDisplayWidth_), Font.size 12, Font.color white, alignRight, moveUp 8] [rt.title |> Element.html |> Element.map (\_ -> NoOp)]
     , column [width (px innerTOCWidth_)] []
    ]


editTools : Model -> Element Msg
editTools model =
    if model.appMode == Editing then
      row [ spacing 6] [
         editingModeButton model
       , newDocumentButtonInHeader
       , saveDocumentButtonInHeader
       , deleteDocumentButtonInHeader model
       , cancelDeleteDocumentButtonInHeader model
       ]
    else
       editingModeButton model

-- TAB-STRIP ON LEFT --

tabStrip : ViewInfo -> { a | visibilityOfTools : Visibility } -> Element Msg
tabStrip viewInfo model =
    column [width (px 30), height(px 200), Background.color (grey 0.1), alignTop ] [
        row [spacing 15, rotate -1.5708,moveLeft 50, moveDown 70] [showToolsButton model, showDocumentListButton model]
    ]




showToolsButton : { a | visibilityOfTools : Visibility } -> Element Msg
showToolsButton model =
  let
      color = if model.visibilityOfTools == Visible then
           red
        else
           buttonGrey
  in
    Input.button [] { onPress = Just (SetToolPanelState Visible)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12] (Element.text "Tools")}


showDocumentListButton : { a | visibilityOfTools : Visibility } -> Element Msg
showDocumentListButton model =
  let
      color = if model.visibilityOfTools == Invisible then
           red
        else
           buttonGrey
  in
   Input.button [ ] { onPress = Just (SetToolPanelState Invisible)
            , label = el [height (px 30), padding 8, Background.color color, Font.color white, Font.size 12] (Element.text "Documents")}



-- FOOTER --

footer : Model -> Element Msg
footer model =
   let
        editorWidth_  =  scale viewInfoEditing.editorWidth  model.windowWidth
        renderedDisplayWidth =   scale viewInfoEditing.renderedDisplayWidth model.windowWidth
        innerTOCWidth_ = scale viewInfoEditing.tocWidth model.windowWidth
     in
       row [ paddingXY 20 0, height (px 30), width (px (model.windowWidth)), Background.color charcoal, Font.color white, spacing 24, Font.size 12] [
             currentAuthorDisplay model
           , currentTime model
           , wordCount model
           , el [] (Element.text <| slug model)
           , el [] (Element.text <| idDisplay model )
           , el [] (Element.text <| model.message)
       ]

slug : Model -> String
slug model =
    case model.currentDocument of
        Nothing -> ""
        Just document -> "Permalink: " ++  Document.slug document

idDisplay  : Model -> String
idDisplay model =
   case model.currentDocument of
     Nothing -> ""
     Just document -> "id = " ++ String.fromInt document.id


currentAuthorDisplay model =
    let
       message = case model.currentUser of
          Nothing -> "Not signed in"
          Just user -> "Signed in as " ++ user.username
     in
      Element.el [Font.color white, Font.size 12]
           (Element.text <| message )


currentTime model =
      Element.el [Font.color white, Font.size 12]
        (Element.text <| "Current time: " ++ Utility.humanTimeHM model.zone model.time)


wordCount model =
    let
         sourceText = case model.currentDocument of
            Just document ->
                document.content
            _ -> ""
         wc =  Utility.wordCount sourceText

    in
      Element.el [Font.color white, Font.size 12] (Element.text <| "Word count: " ++ wc)


status model =
  el [Font.color white, Font.size 12, centerX]
     (Element.text <| "w: " ++ String.fromInt model.windowWidth ++ ", h: " ++ String.fromInt model.windowHeight)

flavors model =
   column [spacing 10, Background.color charcoal, padding 22] [
       el [Font.color white, Font.bold] (Element.text "Markdown Flavor")
      ,  standardMarkdownButton model 93
      , extendedMarkdownButton model 93
      , extendedMathMarkdownButton model 93 ]




---- BUTTONS --



standardMarkdownButton model width =
       let
          bit = (model.option == Standard)
       in
          Input.button (buttonStyleSelected width bit)
              { onPress = Just SelectStandard, label = el [centerX] (Element.text "Standard")}


extendedMarkdownButton model width =
    let
       bit = (model.option == Extended)
    in
        Input.button (buttonStyleSelected width bit)
          { onPress = Just SelectExtended, label = el [centerX] (Element.text "Extended")}


extendedMathMarkdownButton model width =
    let
        bit = (model.option == ExtendedMath)
    in
    Input.button (buttonStyleSelected width bit)
      { onPress = Just SelectExtendedMath, label = el [centerX] (Element.text "ExtendedMath")}



buttonStyleSelected = buttonStyleSelected_ buttonGrey red

buttonStyleSelected_ : Color -> Color -> Int -> Bool -> List (Attr () msg)
buttonStyleSelected_ color color2 width_ bit =
    [ case bit of
        False -> Background.color color
        True -> Background.color color2

    , Font.color white
    , width (px width_)
    , height (px 25)
    , Font.size 12
    , centerX
    ]

textInputStyle w h=
    [ preWrap
    , height <| px <| round h
    , width <| px <| round w
    , clipX
    , paddingXY 12 12
    , Font.size 13
    , paddingXY 8 20
    , Background.color lightGrey
    ,  Border.width 2
    ]

preWrap =
    Element.htmlAttribute (Html.Attributes.attribute "white-space" "pre-wrap")


-- COLORS --

makeGrey g =
    Element.rgb g g g


lightGrey =
    makeGrey 0.95

buttonGrey = grey 0.5

red =  Element.rgb 0.4 0.1 0.1

white = Element.rgb 1 1 1

blue = Element.rgb 0.1 0.1 0.4

grey g = Element.rgb g g g

charcoal = grey 0.3

black = grey 0