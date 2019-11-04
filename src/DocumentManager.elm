module DocumentManager exposing (getById)

import Model exposing (Model, Msg(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Request exposing (RequestMsg(..))
import Utility


getById : String -> String -> Cmd Msg
getById token idString =
    let
        uuid =
            Uuid.fromString idString |> Maybe.withDefault Utility.id0
    in
    Request.publicDocumentsInIdList token [ uuid ] LoadDocument |> Cmd.map Req
