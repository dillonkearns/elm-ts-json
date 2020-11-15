port module GeneratedPort exposing (..)

import Encoder
import Json.Encode
import Ports


sendPort : Ports.ToJs -> Cmd msg
sendPort toJsMsg =
    toJsMsg
        |> Encoder.encoder Ports.toElm
        |> fromElm


port fromElm : Json.Encode.Value -> Cmd msg
