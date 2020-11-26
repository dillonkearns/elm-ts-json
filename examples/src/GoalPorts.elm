module GoalPorts exposing (..)

import TsInterop.Encode as Encode exposing (Encoder, Property, required)


type alias Properties encodesFrom =
    List (Property encodesFrom)


{-| Generates `Ports.sendPresenceHeartbeat : Cmd msg`
-}
sendPresenceHeartbeat : Properties a
sendPresenceHeartbeat =
    []


{-| Generates `Ports.alert : String -> Cmd msg`
-}
alert : Properties String
alert =
    [ required "message" identity Encode.string ]


bugsnag : Properties { context : List String, message : String }
bugsnag =
    [ required "message" .message Encode.string
    , required "context" .context (Encode.list Encode.string)
    ]
