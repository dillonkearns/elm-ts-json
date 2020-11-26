port module GeneratedPort exposing (..)

import Json.Encode
import Ports
import TsInterop.Encode as Encoder


sendPort : Ports.ToJs -> Cmd msg
sendPort toJsMsg =
    toJsMsg
        |> Encoder.encoder Ports.toElm
        |> fromElm


port fromElm : Json.Encode.Value -> Cmd msg
