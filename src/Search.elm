module Search exposing
    ( clearSearchTerms
    , cycleSearchMode
    , do
    , forChildDocuments
    , forPublicDocuments
    , forUsersDocuments
    , getAllDocuments
    , getHelpDocs
    , inputTerms
    )

import Browser.Dom as Dom
import Config
import Element exposing (px, width)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA
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
import Style
import Task


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
                            Request.authorDocumentsWithTitleSorted Config.hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

                SharedDocSearch ->
                    case model.currentUser of
                        Nothing ->
                            Cmd.none

                        Just user ->
                            Request.sharedDocumentsByTitleSorted Config.hasuraToken user.username "" orderByMostRecentFirst GotUserDocuments |> Cmd.map Req

                PublicSearch ->
                    Request.publicDocumentsWithTitle Config.hasuraToken "" |> Cmd.map Req
    in
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }, cmd )


getHelpDocs : Model -> ( Model, Cmd Msg )
getHelpDocs model =
    ( { model | documentListDisplay = ( SearchResults, DequeViewOff ), focusedElement = NoFocus, appMode = Reading, visibilityOfTools = Invisible }
    , Request.publicDocumentsWithTag Config.hasuraToken "usermanual" |> Cmd.map Req
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
                    Request.authorDocumentsWithTitleSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

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
                    Request.sharedDocumentsByTitleSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.sharedDocumentsByTitleSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotUserDocuments |> Cmd.map Req

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
                    Request.authorDocumentsWithTitleSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.documentsWithAuthorAndTagSorted Config.hasuraToken authorIdentifier searchTerm model.sortTerm GotCandidateChildDocuments |> Cmd.map Req

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
                    Request.publicDocumentsWithTitle Config.hasuraToken searchTerm |> Cmd.map Req

                ( KeywordSearch, searchTerm ) ->
                    Request.publicDocumentsWithTag Config.hasuraToken searchTerm |> Cmd.map Req

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



-- UI


cycleSearchMode : Model -> ( Model, Cmd Msg )
cycleSearchMode model =
    let
        nextSearchMode =
            case model.searchMode of
                UserSearch ->
                    PublicSearch

                PublicSearch ->
                    SharedDocSearch

                SharedDocSearch ->
                    UserSearch
    in
    ( { model | searchMode = nextSearchMode }, Cmd.none )


clearSearchTerms model =
    ( { model | searchTerms = "" }, focusSearchBox )


inputTerms model =
    Input.text (Style.inputStyle 200 ++ [ setElementId "search-box" ])
        { onChange = GotSearchTerms
        , text = model.searchTerms
        , placeholder = Nothing
        , label = Input.labelLeft [ Font.size 14, width (px 0) ] (Element.text "")
        }


focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt SetFocusOnSearchBox (Dom.focus "search-box")


setElementId : String -> Element.Attribute msg
setElementId id =
    Element.htmlAttribute <| HA.attribute "id" id
