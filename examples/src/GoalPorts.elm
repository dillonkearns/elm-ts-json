module GoalPorts exposing (..)

import TsInterop.Encode as Encode exposing (Encoder, property)


{-| Generates `Ports.sendPresenceHeartbeat : Cmd msg`
-}
sendPresenceHeartbeat : List ( String, Encoder encodesFrom )
sendPresenceHeartbeat =
    []


{-| Generates `Ports.alert : String -> Cmd msg`
-}
alert : List ( String, Encoder String )
alert =
    [ ( "message", Encode.string ) ]


bugsnag : List ( String, Encoder { a | context : List String, message : String } )
bugsnag =
    [ ( "message", Encode.string |> Encode.map .message )
    , ( "context", Encode.list Encode.string |> Encode.map .context )
    ]
