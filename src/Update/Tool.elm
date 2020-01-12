module Update.Tool exposing (setupToEdit)

import Editor
import Model exposing (Model)


setupToEdit1 =
    identity


setupToEdit : Model -> Model
setupToEdit =
    \model ->
        case model.currentDocument of
            Nothing ->
                model

            Just doc ->
                let
                    st =
                        Editor.getSelectedText model.editor |> Maybe.withDefault ""

                    newEditor =
                        Editor.init Model.config doc.content
                            |> Editor.setSelectedText st
                in
                { model | editor = newEditor }
