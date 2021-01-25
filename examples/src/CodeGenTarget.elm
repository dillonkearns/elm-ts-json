port module CodeGenTarget exposing (..)

import GoalGenerated
import GoalPorts
import GoalPortsFromTs
import Json.Encode as Encode
import ScrollIntoView
import TsInterop.Encode as Encoder
import TsJson.Decode as Decode


{-| TODO only generate for temp file
-}
typeDefs : String
typeDefs =
    GoalGenerated.typeDefs


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
