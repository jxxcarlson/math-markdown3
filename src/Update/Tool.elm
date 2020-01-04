module Update.Tool exposing (setupToEdit, setupToEdit_)

import Buffer exposing (Buffer)
import Editor exposing (EditorConfig, PEEditorMsg, State)
import Model exposing (Model)


setupToEdit : Model -> Model -> Model
setupToEdit model =
    case model.currentDocument of
        Nothing ->
            identity

        Just doc ->
            \model_ -> { model_ | editorBuffer = Buffer.init doc.content, editorState = Editor.init Model.editorConfig }


setupToEdit_ : Model -> Model
setupToEdit_ =
    \model ->
        case model.currentDocument of
            Nothing ->
                model

            Just doc ->
                { model | editorBuffer = Buffer.init doc.content, editorState = Editor.init Model.editorConfig }
