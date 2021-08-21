module AndThenExample exposing (main)

import Browser
import Html exposing (Html, pre, text)
import TsJson.Codec
import TsJson.Decode exposing (andThenInit)
import TsJson.Encode

import Browser
type alias Model =
    ()

view : Model -> Html ()
view _ =
    let
        result =
            let
                v1codec =
                    TsJson.Decode.andThenDecoder (TsJson.Decode.field "payload" TsJson.Decode.string)

                v2Codec =
                    (TsJson.Codec.object identity |> TsJson.Codec.field "data" identity TsJson.Codec.string |> TsJson.Codec.buildObject)
                example =
                    andThenInit
                        (\v1Decoder v2PlusDecoder version ->
                            case version of
                                1 -> v1Decoder
                                _ -> v2PlusDecoder
                        )
                        |> v1codec

                versionCodec =
                    (TsJson.Codec.object identity |> TsJson.Codec.field "version" identity TsJson.Codec.int |> TsJson.Codec.buildObject)
            in

            (TsJson.Codec.andThen example v2Codec versionCodec
                |> TsJson.Codec.encoder
                |> TsJson.Encode.runExample) "test"
                |> Debug.toString
        --> { output = "{\"data\":\"test\"}"
        --> , tsType = "{ data : string; version : number }" }
    in
    pre [] [text result ]


main : Program () Model ()
main =
    Browser.sandbox
        { init = ()
        , view = view
        , update = (\_ -> identity)
        }

