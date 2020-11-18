port module CodeGenTarget exposing (..)

import GoalPorts
import GoalPortsFromTs
import Json.Encode as Encode
import TsInterop.Decode as Decode
import TsInterop.Encode as Encoder


{-| TODO only generate for temp file
-}
typeDefs : String
typeDefs =
    Encoder.unionTypeDefToString
        [ ( "alert", GoalPorts.alert |> Encoder.rawType )
        , ( "bugsnag", GoalPorts.bugsnag |> Encoder.rawType )
        , ( "sendPresenceHeartbeat", GoalPorts.sendPresenceHeartbeat |> Encoder.rawType )
        ]


allTypeDefs : { fromElm : String, toElm : String, flags : String }
allTypeDefs =
    { fromElm = typeDefs
    , toElm = typeDefs
    , flags = flagDecoder
    }


flagDecoder : String
flagDecoder =
    GoalPortsFromTs.flags
        |> Decode.tsTypeToString


type Severity
    = Info
    | Warning
    | Error


port fromElm : Encode.Value -> Cmd msg


port log :
    { fromElm : String
    , toElm : String
    , flags : String
    }
    -> Cmd msg


main : Program () () ()
main =
    Platform.worker
        { init =
            \() ->
                ( ()
                , log allTypeDefs
                )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \() -> Sub.none
        }
