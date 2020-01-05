module Update.UI exposing (setModeToEditing, setModeToReading, setUserMode, toggleKeyboardTools)

--import Buffer exposing (Buffer)
--import Editor exposing (EditorConfig, PEEditorMsg, State)

import Model exposing (AppMode(..), EditMode(..), Model, Msg(..), UserState, Visibility(..))
import Update.Master
import Update.Tool


setModeToReading : Model -> ( Model, Cmd Msg )
setModeToReading model =
    ( { model | appMode = Reading, visibilityOfTools = Invisible }, Cmd.none )


setModeToEditing : Model -> EditMode -> ( Model, Cmd Msg )
setModeToEditing model editMode =
    ( { model
        | appMode = Editing editMode
        , visibilityOfTools = Invisible
        , documentOutline = Update.Master.setupOutline_ model
      }
        |> Update.Tool.setupToEdit
    , Cmd.none
    )


setUserMode : Model -> UserState -> ( Model, Cmd msg )
setUserMode model s =
    ( { model | appMode = UserMode s }, Cmd.none )


toggleKeyboardTools : Model -> ( Model, Cmd Msg )
toggleKeyboardTools model =
    if model.appMode /= Editing StandardEditing then
        ( model, Cmd.none )

    else
        case model.visibilityOfTools of
            Visible ->
                ( { model | visibilityOfTools = Invisible }, Cmd.none )

            Invisible ->
                ( { model | visibilityOfTools = Visible }, Cmd.none )
