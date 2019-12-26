module View.Common exposing (RenderedDocumentRecord, ViewInfo, affine, appModeAsString, scale, setElementId, setHtmlId, translate)

import Element
import Html exposing (Html)
import Html.Attributes as HA
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


type alias ViewInfo =
    { toolStripWidth : Float
    , docListWidth : Float
    , editorWidth : Float
    , renderedDisplayWidth : Float
    , tocWidth : Float
    , vInset : Float
    , hExtra : Float
    }


type alias RenderedDocumentRecord msg =
    { document : Html msg, title : Html msg, toc : Html msg }



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
