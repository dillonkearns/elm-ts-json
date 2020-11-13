module GoalPorts exposing (..)

import TsPort exposing (property)


{-| Generates `Ports.sendPresenceHeartbeat : Cmd msg`
-}
sendPresenceHeartbeat : TsPort.ObjectBuilder encodesFrom
sendPresenceHeartbeat =
    TsPort.build


{-| Generates `Ports.alert : String -> Cmd msg`
-}
alert : TsPort.ObjectBuilder String
alert =
    TsPort.build
        |> property "message" TsPort.string


bugsnag : TsPort.ObjectBuilder { a | context : List String, message : String }
bugsnag =
    TsPort.build
        |> property "message" (TsPort.string |> TsPort.map .message)
        |> property "context" (TsPort.list TsPort.string |> TsPort.map .context)
