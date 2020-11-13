port module GoalGenerated exposing
    ( alert
    , bugsnag
    , sendPresenceHeartbeat
    )

import GoalPorts
import Json.Encode
import TsPort


sendPresenceHeartbeat : Cmd msg
sendPresenceHeartbeat =
    ()
        |> TsPort.encodeProVariant "sendPresenceHeartbeat" GoalPorts.sendPresenceHeartbeat
        |> fromElm


alert : String -> Cmd msg
alert argument =
    argument
        |> TsPort.encodeProVariant "alert" GoalPorts.alert
        |> fromElm


bugsnag : { a | context : List String, message : String } -> Cmd msg
bugsnag argument =
    argument
        |> TsPort.encodeProVariant "bugsnag" GoalPorts.bugsnag
        |> fromElm


{-| TODO only generate for temp file
-}
typeDefs : String
typeDefs =
    TsPort.customTypeDefToString
        [ ( "alert", GoalPorts.alert |> TsPort.rawType )
        , ( "bugsnag", GoalPorts.bugsnag |> TsPort.rawType )
        , ( "sendPresenceHeartbeat", GoalPorts.sendPresenceHeartbeat |> TsPort.rawType )
        ]


port fromElm : Json.Encode.Value -> Cmd msg
