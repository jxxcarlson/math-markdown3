module KeyboardManager exposing (handleKey, headKey, keyboardGateway)

import Keyboard exposing (Key(..))
import Model exposing (AppMode(..), EditMode(..), FocusedElement(..), Model, Msg(..), UserState(..))
import Search
import Update.Document
import Update.UI


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
                Search.forChildDocuments model

            False ->
                Search.do newModel

    else
        ( { model | pressedKeys = pressedKeys }, Cmd.none )


handleKey : Model -> Key -> ( Model, Cmd Msg )
handleKey model key =
    case key of
        Character "a" ->
            Search.getAllDocuments model

        Character "e" ->
            Update.UI.setModeToEditing model StandardEditing

        --        Character "f" ->
        --            ( model, focusSearchBox )
        Character "h" ->
            Search.getHelpDocs model

        Character "f" ->
            Search.clearSearchTerms model

        Character "n" ->
            Update.Document.makeNewDocument model

        Character "r" ->
            Update.UI.setModeToReading model

        Character "s" ->
            Update.Document.saveDocument model

        Character "t" ->
            Update.UI.toggleKeyboardTools model

        Character "u" ->
            case model.currentUser of
                Nothing ->
                    Update.UI.setUserMode model SignInState

                _ ->
                    Update.UI.setUserMode model SignedInState

        _ ->
            ( model, Cmd.none )


headKey : List Key -> Key
headKey keyList =
    keyList
        |> List.filter (\item -> item /= Control && item /= Character "^")
        |> List.head
        |> Maybe.withDefault F20
