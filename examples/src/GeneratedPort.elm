port module GeneratedPort exposing (..)

import Json.Encode
import Ports
import TsPort


sendPort : Ports.ToJs -> Cmd msg
sendPort toJsMsg =
    toJsMsg
        |> TsPort.encoder Ports.toElm
        |> fromElm


port fromElm : Json.Encode.Value -> Cmd msg
