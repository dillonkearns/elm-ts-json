module Ports exposing (..)

import TsPort exposing (property)


type ToJs
    = SendPresenceHeartbeat
    | Alert String


toElm : TsPort.Encoder ToJs
toElm =
    TsPort.custom
        (\vSendHeartbeat vAlert value ->
            case value of
                SendPresenceHeartbeat ->
                    vSendHeartbeat

                Alert string ->
                    vAlert string
        )
        |> TsPort.variant0 "SendPresenceHeartbeat"
        |> TsPort.objectVariant "Alert"
            (TsPort.build
                |> property "message" TsPort.string
            )
        |> TsPort.buildCustom
