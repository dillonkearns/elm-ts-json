module Ports exposing (..)

import Encoder exposing (property)


type ToJs
    = SendPresenceHeartbeat
    | Alert String


toElm : Encoder.Encoder ToJs
toElm =
    Encoder.custom
        (\vSendHeartbeat vAlert value ->
            case value of
                SendPresenceHeartbeat ->
                    vSendHeartbeat

                Alert string ->
                    vAlert string
        )
        |> Encoder.variant0 "SendPresenceHeartbeat"
        |> Encoder.objectVariant "Alert"
            (Encoder.build
                |> property "message" Encoder.string
            )
        |> Encoder.buildCustom
