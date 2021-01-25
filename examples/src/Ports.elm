module Ports exposing (..)

import TsJson.Encode as Encoder exposing (required, string)


type ToJs
    = SendPresenceHeartbeat
    | Alert String


toElm : Encoder.Encoder ToJs
toElm =
    Encoder.union
        (\vSendHeartbeat vAlert value ->
            case value of
                SendPresenceHeartbeat ->
                    vSendHeartbeat

                Alert string ->
                    vAlert string
        )
        |> Encoder.variant0 "SendPresenceHeartbeat"
        |> Encoder.variantObject "Alert" [ required "message" identity string ]
        |> Encoder.buildUnion
