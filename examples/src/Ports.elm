module Ports exposing (..)

import TsInterop.Encode as Encoder exposing (property)


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
        |> Encoder.variantObject "Alert" [ ( "message", Encoder.string ) ]
        |> Encoder.buildUnion
