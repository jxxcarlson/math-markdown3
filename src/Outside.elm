port module Outside exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfo
    , sendInfo
    )

import Document exposing (Document)
import Editor
import Json.Decode as D
import Json.Encode as E
import User exposing (OutsideUser, User)


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


type alias GenericOutsideData =
    { tag : String, data : E.Value }


type InfoForElm
    = UserDataFromOutside OutsideUser
    | GotSelection String


type InfoForOutside
    = UserData E.Value
    | AskToReconnectUser E.Value
    | GetTextSelectionFromOutside E.Value


getInfo : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfo tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                "ReconnectUser" ->
                    case D.decodeValue User.outsideUserDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| UserDataFromOutside result

                        Err e ->
                            onError <| ""

                "GotSelection" ->
                    case D.decodeValue Editor.selectionDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| GotSelection result

                        Err e ->
                            onError <| ""

                _ ->
                    onError <| "Unexpected info from outside"
        )


sendInfo : InfoForOutside -> Cmd msg
sendInfo info =
    case info of
        UserData value ->
            infoForOutside { tag = "UserData", data = value }

        AskToReconnectUser value ->
            infoForOutside { tag = "AskToReconnectUser", data = E.null }

        GetTextSelectionFromOutside value ->
            infoForOutside { tag = "GetSelection", data = E.null }