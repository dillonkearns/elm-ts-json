port module Example exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Interop toJs fromJs
    = Interop


decoder : Interop toJs fromJs -> Decoder fromJs
decoder interop =
    Debug.todo ""


encoder : Interop toJs fromJs -> (toJs -> Encode.Value)
encoder interop =
    Debug.todo ""


ports : Interop toJs fromJs -> (toJs -> msg) -> (Result Decode.Error fromJs -> msg) -> ( Cmd msg, Sub msg )
ports interop toJsMsg fromJsMsg =
    ( --toJsLowLevel
      --    |> Cmd.map
      --        (\decodeValue ->
      --            Decode.decodeValue (decoder interop) decodeValue
      --                |> toJsMsg
      --        )
      Debug.todo ""
    , fromJsPort (\value -> Decode.decodeValue (decoder interop) value |> fromJsMsg)
    )


port toJsLowLevel : Decode.Value -> Cmd msg


port fromJsLowLevel : (Encode.Value -> msg) -> Sub msg


port toJsPort : Encode.Value -> Cmd msg


port fromJsPort : (Decode.Value -> msg) -> Sub msg


worker =
    Platform.worker
        { init = init
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type alias Flags =
    ()


type alias Msg =
    Never


init : () -> ( Model, Cmd Msg )
init () =
    ( (), toJsPort (Encode.string "TODO - typedef") )
