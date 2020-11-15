module GoalPorts exposing (..)

import TsInterop.Encode as Encode exposing (property)


{-| Generates `Ports.sendPresenceHeartbeat : Cmd msg`
-}
sendPresenceHeartbeat : Encode.ObjectBuilder encodesFrom
sendPresenceHeartbeat =
    Encode.build


{-| Generates `Ports.alert : String -> Cmd msg`
-}
alert : Encode.ObjectBuilder String
alert =
    Encode.build
        |> property "message" Encode.string


bugsnag : Encode.ObjectBuilder { a | context : List String, message : String }
bugsnag =
    Encode.build
        |> property "message" (Encode.string |> Encode.map .message)
        |> property "context" (Encode.list Encode.string |> Encode.map .context)
