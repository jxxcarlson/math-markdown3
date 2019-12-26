module View.User exposing (view)

import BoundedDeque
import Button
import Data
import Document exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Markdown.Elm
import Markdown.Option exposing (Option(..))
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
import Prng.Uuid as Uuid exposing (Uuid)
import Search
import String.Interpolate exposing (interpolate)
import Style
import TocZ exposing (TocMsg(..), viewZ)
import User exposing (User)
import Utility.View
import View.Common exposing (ViewInfo)
import View.Widget



-- USER PAGE --


type alias ViewInfoUserPage =
    { lhsFraction : Float, rhsFraction : Float, vInset : Float }


view : ViewInfoUserPage -> Model -> Element Msg
view viewInfo model =
    let
        h_ =
            View.Common.translate -viewInfo.vInset model.windowHeight
    in
    column [ Background.color (Style.makeGrey 0.35), Font.color Style.white ]
        [ userPageHeader viewInfo model
        , row [ height (px h_), Font.size 14 ] [ lhsViewInfoPage viewInfo model, rhsViewInfoPage viewInfo model ]
        , userPageFooter model
        ]


lhsViewInfoPage viewInfo model =
    let
        w =
            View.Common.scale viewInfo.lhsFraction model.windowWidth

        h =
            View.Common.translate -viewInfo.vInset model.windowHeight
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
            View.Common.scale (0.7 * viewInfo.rhsFraction) model.windowWidth

        w2 =
            View.Common.scale (0.3 * viewInfo.rhsFraction) model.windowWidth

        h =
            View.Common.translate -viewInfo.vInset model.windowHeight

        rt : { title : Html msg, toc : Html msg, document : Html msg }
        rt =
            Markdown.Elm.toHtmlWithExternaTOC ExtendedMath Data.rhsUserText
    in
    row []
        [ column []
            [ column [] [ rt.title |> Element.html ]
            , column [ width (px w1), height (px h), padding 36, scrollbarY, Background.color (Style.makeGrey 0.9), Font.color (Style.makeGrey 0.1) ]
                [ rt.document |> Element.html ]
            ]
        , column [ width (px w2), height (px h), padding 12, scrollbarY, Background.color (Style.makeGrey 0.8), Font.color (Style.makeGrey 0.1) ]
            [ rt.toc |> Element.html ]
        ]


userPageFooter : Model -> Element Msg
userPageFooter model =
    row [ paddingXY 20 0, height (px 30), width (px model.windowWidth), Background.color Style.charcoal, Font.color Style.white, spacing 24, Font.size 12 ]
        [ el [] (Element.text <| View.Common.appModeAsString model)
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
            View.Common.scale viewInfo.lhsFraction model.windowWidth

        rhsWidth =
            View.Common.scale viewInfo.rhsFraction model.windowWidth
    in
    row [ height (px 45), width (px model.windowWidth), Background.color Style.charcoal ]
        [ View.Widget.modeButtonStrip model lhsWidth
        , column [ width (px rhsWidth) ] []
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
