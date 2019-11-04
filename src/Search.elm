module Search exposing (getAllDocuments)

import Config
import Model
    exposing
        ( AppMode(..)
        , DequeViewState(..)
        , DocumentListType(..)
        , FocusedElement(..)
        , Model
        , Msg(..)
        , SearchMode(..)
        , Visibility(..)
        )
import Request exposing (AuthReply(..), GraphQLResponse(..), RequestMsg(..), orderByMostRecentFirst, orderByTitleAsc)


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
                            Request.authorDocumentsWithTitleSorted Config.data.hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

                SharedDocSearch ->
                    case model.currentUser of
                        Nothing ->
                            Cmd.none

                        Just user ->
                            Request.sharedDocumentsByTitleSorted Config.data.hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

                PublicSearch ->
                    Request.publicDocumentsWithTitle Config.data.hasuraToken "" |> Cmd.map Req
    in
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )
