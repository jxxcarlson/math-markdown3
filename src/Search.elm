module Search exposing
    ( do
    , forChildDocuments
    , forPublicDocuments
    , forUsersDocuments
    , getAllDocuments
    , getHelpDocs
    )

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
        , SearchType(..)
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


getHelpDocs : Model -> ( Model, Cmd Msg )
getHelpDocs model =
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }
    , Request.publicDocumentsWithTag Config.data.hasuraToken "usermanual" |> Cmd.map Req
    )


do : Model -> ( Model, Cmd Msg )
do model =
    case model.searchMode of
        UserSearch ->
            forUsersDocuments model

        SharedDocSearch ->
            searchForSharedDocuments model

        PublicSearch ->
            forPublicDocuments model


forUsersDocuments : Model -> ( Model, Cmd Msg )
forUsersDocuments model =
    let
        authorIdentifier =
            model.currentUser |> Maybe.map .username |> Maybe.withDefault "__nobodyHere__"

        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.authorDocumentsWithTitleSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


searchForSharedDocuments : Model -> ( Model, Cmd Msg )
searchForSharedDocuments model =
    let
        authorIdentifier =
            model.currentUser |> Maybe.map .username |> Maybe.withDefault "__nobodyHere__"

        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.sharedDocumentsByTitleSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.sharedDocumentsByTitleSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( { model
        | documentListDisplay = ( SearchResults, DequeViewOff )
        , focusedElement = NoFocus
        , appMode = Reading
        , visibilityOfTools = Invisible
      }
    , cmd
    )


forChildDocuments : Model -> ( Model, Cmd Msg )
forChildDocuments model =
    let
        authorIdentifier =
            model.currentUser |> Maybe.map .username |> Maybe.withDefault "__nobodyHere__"

        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.authorDocumentsWithTitleSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted Config.data.hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( model, cmd )


forPublicDocuments : Model -> ( Model, Cmd Msg )
forPublicDocuments model =
    let
        cmd =
            case parseSearchTerm model.searchTerms of
                ( TitleSearch, searchTerm ) ->
                    Request.publicDocumentsWithTitle Config.data.hasuraToken searchTerm |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.publicDocumentsWithTag Config.data.hasuraToken searchTerm |> Cmd.map Req

                ( NoSearchTerm, _ ) ->
                    Cmd.none
    in
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


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


stringValueOfSearchType : String -> SearchType
stringValueOfSearchType str =
    case str of
        "k" ->
            KeywordSearch

        _ ->
            TitleSearch
