port module Outside exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfo
    , pushUrl
    , sendInfo
    )

import BoundedDeque exposing (BoundedDeque)
import Document exposing (Document)
import EditorTools
import Json.Decode as D
import Json.Encode as E
import Prng.Uuid as Uuid exposing (Uuid)
import User exposing (OutsideUser, User)


port pushUrl : String -> Cmd msg


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


type alias GenericOutsideData =
    { tag : String, data : E.Value }


type InfoForElm
    = UserDataFromOutside OutsideUser
    | GotSelection String
    | GotSelectionForSync String
    | UuidList (List Uuid)
    | GotClipboard String


type InfoForOutside
    = AskToReconnectUser E.Value
    | UserData E.Value
    | DestroyUserData E.Value
    | AskForDequeData E.Value
    | DequeData E.Value
    | GetTextSelectionFromOutside E.Value
    | GetSelectionForSyncOutside E.Value
    | ScrollToLine E.Value
    | AskForClipBoard E.Value
    | WriteToClipBoard String


stringOfInfoForOutside : InfoForOutside -> String
stringOfInfoForOutside info =
    case info of
        AskToReconnectUser _ ->
            "AskToReconnectUser"

        UserData _ ->
            "UserData"

        DestroyUserData _ ->
            "DestroyUserData"

        AskForDequeData _ ->
            "AskForDequeData"

        DequeData _ ->
            "DequeData"

        GetTextSelectionFromOutside _ ->
            "GetTextSelectionFromOutside"

        GetSelectionForSyncOutside _ ->
            "GetSelectionForSyncOutside"

        ScrollToLine _ ->
            "ScrollToLine"

        AskForClipBoard _ ->
            "AskForClipBoard"

        WriteToClipBoard _ ->
            "WriteToClipBoard"


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
                    case D.decodeValue EditorTools.selectionDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| GotSelection result

                        Err e ->
                            onError <| ""

                "GotSelectionForSync" ->
                    case D.decodeValue EditorTools.selectionDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| GotSelectionForSync result

                        Err e ->
                            onError <| ""

                "UuidList" ->
                    case D.decodeValue Document.uuidListDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| UuidList result

                        Err e ->
                            onError <| ""

                "GotClipboard" ->
                    case D.decodeValue clipboardDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| GotClipboard result

                        Err e ->
                            onError <| ""

                _ ->
                    onError <| "Unexpected info from outside"
        )


sendInfo : InfoForOutside -> Cmd msg
sendInfo info =
    let
        _ =
            Debug.log "sendInfo" (stringOfInfoForOutside info)
    in
    case info of
        UserData value ->
            infoForOutside { tag = "UserData", data = value }

        DestroyUserData value ->
            infoForOutside { tag = "DestroyUserData", data = value }

        DequeData value ->
            infoForOutside { tag = "DequeData", data = value }

        AskForDequeData value ->
            infoForOutside { tag = "AskForDequeData", data = E.null }

        AskToReconnectUser value ->
            infoForOutside { tag = "AskToReconnectUser", data = E.null }

        GetTextSelectionFromOutside value ->
            infoForOutside { tag = "GetSelection", data = E.null }

        GetSelectionForSyncOutside value ->
            let
                _ =
                    Debug.log "GetSelectionForSyncOutside" "HERE!"
            in
            infoForOutside { tag = "GetSelectionForSyncOutside", data = E.null }

        ScrollToLine value ->
            infoForOutside { tag = "ScrollToLine", data = value }

        AskForClipBoard value ->
            infoForOutside { tag = "AskForClipBoard", data = E.null }

        WriteToClipBoard str ->
            infoForOutside { tag = "WriteToClipboard", data = E.string str }



-- DECODERS --


clipboardDecoder : D.Decoder String
clipboardDecoder =
    --    D.field "data" D.string
    D.string
