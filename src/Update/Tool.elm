module Update.Tool exposing (setupToEdit)

import Buffer exposing (Buffer)
import Editor exposing (EditorConfig, PEEditorMsg, State)
import Model exposing (Model)


setupToEdit : Model -> Model
setupToEdit =
    \model ->
        case model.currentDocument of
            Nothing ->
                model

            Just doc ->
                let
                    st =
                        Editor.getSelectedText model.editorState |> Maybe.withDefault ""
                in
                { model
                    | editorBuffer = Buffer.init doc.content
                    , editorState =
                        Editor.init Model.editorConfig
                            |> Editor.setSelectedText st
                }
