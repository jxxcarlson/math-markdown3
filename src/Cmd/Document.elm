module Cmd.Document exposing
    ( getById
    , getBySlug
    , getUserDocumentsAtSignIn
    , idToScrollRenderedTxt
    , processUrl
    , pushDocument
    , renderAstFor
    , resetViewportOfEditor
    , resetViewportOfRenderedText
    ,  scrollIfNeeded
       --    , sendDequeOutside

    , sendDequeOutside
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
import Process
import Render exposing (RenderingData)
import Request exposing (RequestMsg(..), orderByMostRecentFirst)
import Task exposing (Task)
import Url exposing (Url)
import User exposing (User)
import Utility


{-| Get a public document by its Uuid.
-}
getById : String -> String -> Cmd Msg
getById token idString =
    let
        uuid =
            Uuid.fromString idString |> Maybe.withDefault Utility.id0
    in
    Request.publicDocumentsInIdList token [ uuid ] LoadDocument |> Cmd.map Req


{-| Get a public document by its slug.
-}
getBySlug : String -> String -> Cmd Msg
getBySlug token slug =
    Request.publicDocumentsBySlug token slug LoadDocument |> Cmd.map Req


{-| Store the recent documents deque in local storage via ports
-}
sendDequeOutside : BoundedDeque Document -> Cmd Msg
sendDequeOutside deque =
    let
        data : E.Value
        data =
            deque
                |> Document.idListOfDeque
                |> Document.encodeStringList "deque"
    in
    Outside.sendInfo (Outside.DequeData data) |> Cmd.map Req


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
                    getBySlug Config.hasuraToken f

                ( IdRef, f ) ->
                    getById Config.hasuraToken f

                ( SubdocIdRef, _ ) ->
                    Cmd.none



-- VIEWPORT


scrollIfNeeded : String -> Cmd Msg
scrollIfNeeded tag =
    Task.attempt ScrollAttempted
        (Dom.getElement tag
            |> Task.andThen (\info -> Dom.setViewportOf idToScrollRenderedTxt 0 (info.element.y - info.element.height - 40))
        )


resetViewportOfRenderedText : Cmd Msg
resetViewportOfRenderedText =
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf idToScrollRenderedTxt -100 0)


resetViewportOfEditor : Cmd Msg
resetViewportOfEditor =
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf "_editor_" 0 0)


setViewportForElement : String -> Cmd Msg
setViewportForElement id =
    Dom.getViewportOf idToScrollRenderedTxt
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
            viewport.viewport.y + element.element.y - 230
    in
    Task.attempt (\_ -> NoOp) (Dom.setViewportOf "__rt_scroll__" 0 y)



-- SCROLL RENDERED TEXT


idToScrollRenderedTxt : String
idToScrollRenderedTxt =
    "__rt_scroll2__"



-- SIGN IN


getUserDocumentsAtSignIn : User -> Cmd Msg
getUserDocumentsAtSignIn user =
    Request.authorDocumentsWithTitleSorted Config.hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req



-- RENDER


renderAstFor : ( Int, Int ) -> RenderingData Msg -> Cmd Msg
renderAstFor selectedId rd =
    Process.sleep 10
        |> Task.andThen
            (\_ ->
                Process.sleep 100
                    |> Task.andThen (\_ -> Task.succeed (Render.render selectedId rd))
            )
        |> Task.perform GotSecondPart
