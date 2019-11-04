module Cmd.Document exposing
    ( getById
    , getBySlug
    , masterId
    , processUrl
    , pushDocument
    , resetViewportOfEditor
    , resetViewportOfRenderedText
    , scrollIfNeeded
    , sendDequeOutside
    , sendDequeOutside_
    , setViewPortForSelectedLine
    , setViewportForElement
    )

import AppNavigation exposing (NavigationType(..))
import BoundedDeque exposing (BoundedDeque)
import Browser.Dom as Dom
import Config
import Document exposing (Document)
import Json.Encode as E
import Maybe.Extra
import Model exposing (Model, Msg(..))
import Outside
import Prng.Uuid as Uuid exposing (Uuid)
import Request exposing (RequestMsg(..))
import Task exposing (Task)
import Url exposing (Url)
import Utility


getById : String -> String -> Cmd Msg
getById token idString =
    let
        uuid =
            Uuid.fromString idString |> Maybe.withDefault Utility.id0
    in
    Request.publicDocumentsInIdList token [ uuid ] LoadDocument |> Cmd.map Req


getBySlug : String -> String -> Cmd Msg
getBySlug token slug =
    Request.publicDocumentsBySlug token slug LoadDocument |> Cmd.map Req



-- OUTSIDE HELPERS --


sendDequeOutside : Model -> Cmd Msg
sendDequeOutside model =
    let
        data : E.Value
        data =
            model.deque
                |> Document.idListOfDeque
                |> Document.encodeStringList "deque"
    in
    Outside.sendInfo (Outside.DequeData data) |> Cmd.map Req


sendDequeOutside_ : BoundedDeque Document -> Cmd Msg
sendDequeOutside_ deque =
    let
        data : E.Value
        data =
            deque
                |> Document.idListOfDeque
                |> Document.encodeStringList "deque"
    in
    Outside.sendInfo (Outside.DequeData data) |> Cmd.map Req


scrollIfNeeded : String -> Cmd Msg
scrollIfNeeded tag =
    Task.attempt ScrollAttempted
        (Dom.getElement tag
            |> Task.andThen (\info -> Dom.setViewportOf "__rt_scroll__" 0 (info.element.y - info.element.height - 40))
        )


pushDocument : Document -> Cmd Msg
pushDocument document =
    Outside.pushUrl <| "/" ++ Uuid.toString document.id


processUrl : String -> Cmd Msg
processUrl urlString =
    let
        url =
            Url.fromString urlString

        maybeFrag =
            Maybe.map .fragment url |> Maybe.Extra.join
    in
    case maybeFrag of
        Nothing ->
            Cmd.none

        Just frag ->
            case AppNavigation.classify frag of
                ( TocRef, f ) ->
                    Cmd.none

                ( DocRef, f ) ->
                    getBySlug Config.data.hasuraToken f

                ( IdRef, f ) ->
                    getById Config.data.hasuraToken f

                ( SubdocIdRef, _ ) ->
                    Cmd.none



-- VIEWPORT


resetViewportOfRenderedText : Cmd Msg
resetViewportOfRenderedText =
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf masterId -100 0)


resetViewportOfEditor : Cmd Msg
resetViewportOfEditor =
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf "_editor_" 0 0)


setViewportForElement : String -> Cmd Msg
setViewportForElement id =
    Dom.getViewportOf masterId
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


masterId =
    "__rt_scroll__"
