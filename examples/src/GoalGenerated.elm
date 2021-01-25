port module GoalGenerated exposing
    ( alert
    , bugsnag
    , sendPresenceHeartbeat
    , typeDefs
    )

import GoalPorts
import Json.Encode
import ScrollIntoView
import TsJson.Encode as Encode


sendPresenceHeartbeat : Cmd msg
sendPresenceHeartbeat =
    ()
        |> encodePro "sendPresenceHeartbeat" GoalPorts.sendPresenceHeartbeat
        |> fromElm


alert : String -> Cmd msg
alert argument =
    argument
        |> encodePro "alert" GoalPorts.alert
        |> fromElm


bugsnag : { a | context : List String, message : String } -> Cmd msg
bugsnag argument =
    argument
        |> encodePro "bugsnag" GoalPorts.bugsnag
        |> fromElm


scrollIntoView :
    { options :
        { behavior : Maybe ScrollIntoView.Behavior, block : Maybe ScrollIntoView.Alignment, inline : Maybe ScrollIntoView.Alignment }
    , id : String
    }
    -> Cmd msg
scrollIntoView argument =
    argument
        |> encodePro "scrollIntoView" GoalPorts.scrollIntoView
        |> fromElm


{-| TODO only generate for temp file
-}
typeDefs : String
typeDefs =
    toTypeDefs
        [ ( "alert", GoalPorts.alert |> Encode.typeDef )
        , ( "bugsnag", GoalPorts.bugsnag |> Encode.typeDef )
        , ( "sendPresenceHeartbeat", GoalPorts.sendPresenceHeartbeat |> Encode.typeDef )
        ]


toTypeDefs : List ( String, String ) -> String
toTypeDefs variants =
    List.map individualThing variants
        |> String.join " | "


individualThing : ( String, String ) -> String
individualThing ( tagName, encoderType ) =
    "{ tag : '" ++ tagName ++ "'; data : " ++ encoderType


port fromElm : Json.Encode.Value -> Cmd msg


encodePro : String -> Encode.Encoder input -> input -> Json.Encode.Value
encodePro tagName encoder =
    Encode.object
        [ Encode.required tagName identity encoder
        , Encode.required "tag" identity (Encode.literal (Json.Encode.string tagName))
        ]
        |> Encode.encoder
