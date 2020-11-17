port module CodeGenTarget exposing (..)

import GoalPorts
import Json.Encode
import TsInterop.Encode as Encode


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


port log : String -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init = \() -> ( (), log typeDefs )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \() -> Sub.none
        }
