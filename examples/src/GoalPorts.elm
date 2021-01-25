module GoalPorts exposing (..)

import ScrollIntoView
import TsJson.Encode as Encode exposing (Encoder, Property, optional, required)


type alias Properties input =
    List (Property input)


{-| Generates `Ports.sendPresenceHeartbeat : Cmd msg`
-}
sendPresenceHeartbeat : Encoder a
sendPresenceHeartbeat =
    Encode.null


{-| Generates `Ports.alert : String -> Cmd msg`
-}
alert : Encoder String
alert =
    Encode.object
        [ required "message" identity Encode.string ]


bugsnag : Encoder { context : List String, message : String }
bugsnag =
    Encode.object
        [ required "message" .message Encode.string
        , required "context" .context (Encode.list Encode.string)
        ]


scrollIntoView :
    Encoder
        { options :
            { behavior : Maybe ScrollIntoView.Behavior, block : Maybe ScrollIntoView.Alignment, inline : Maybe ScrollIntoView.Alignment }
        , id : String
        }
scrollIntoView =
    Encode.object
        [ required "options" .options ScrollIntoView.encoder
        , required "id" .id Encode.string
        ]
