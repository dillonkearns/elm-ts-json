port module GoalGenerated exposing
    ( alert
    , bugsnag
    , sendPresenceHeartbeat
    )

import GoalPorts
import Json.Encode
import ScrollIntoView
import TsInterop.Encode as Encode


sendPresenceHeartbeat : Cmd msg
sendPresenceHeartbeat =
    ()
        |> Encode.encodeProVariant "sendPresenceHeartbeat" GoalPorts.sendPresenceHeartbeat
        |> fromElm


alert : String -> Cmd msg
alert argument =
    argument
        |> Encode.encodeProVariant "alert" GoalPorts.alert
        |> fromElm


bugsnag : { a | context : List String, message : String } -> Cmd msg
bugsnag argument =
    argument
        |> Encode.encodeProVariant "bugsnag" GoalPorts.bugsnag
        |> fromElm


scrollIntoView :
    { options :
        { behavior : Maybe ScrollIntoView.Behavior, block : Maybe ScrollIntoView.Alignment, inline : Maybe ScrollIntoView.Alignment }
    , id : String
    }
    -> Cmd msg
scrollIntoView argument =
    argument
        |> Encode.encodeProVariant "scrollIntoView" GoalPorts.scrollIntoView
        |> fromElm


{-| TODO only generate for temp file
-}
typeDefs : String
typeDefs =
    Encode.unionTypeDefToString
        [ ( "alert", GoalPorts.alert |> Encode.rawType )
        , ( "bugsnag", GoalPorts.bugsnag |> Encode.rawType )
        , ( "sendPresenceHeartbeat", GoalPorts.sendPresenceHeartbeat |> Encode.rawType )
        ]


port fromElm : Json.Encode.Value -> Cmd msg
