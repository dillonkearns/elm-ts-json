module TsJson.DecodePipelineTests exposing (all)

import Expect exposing (Expectation)
import Json.Decode as Decode
import Test exposing (..)
import TsJson.Decode as TsDecode exposing (Decoder, null, string)
import TsJson.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import TsType


{-| Run some JSON through a Decoder and return the result.
-}
runWith : String -> Decoder a -> { decoded : Result String a, tsType : String }
runWith string decoder =
    { decoded =
        Decode.decodeString (TsDecode.decoder decoder) string
            |> Result.mapError Decode.errorToString
    , tsType = TsDecode.tsType decoder |> TsType.toString
    }


isError : Result err ok -> Bool
isError result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


expectErr : { a | decoded : Result err ok } -> Expectation
expectErr result =
    isError result.decoded
        |> Expect.true ("Expected an Err but got " ++ Debug.toString result)


all : Test
all =
    describe
        "Json.Decode.Pipeline"
        [ test "should decode basic example" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> required "a" string
                    |> required "b" string
                    |> runWith """{"a":"foo","b":"bar"}"""
                    |> Expect.equal
                        { decoded = Ok ( "foo", "bar" )
                        , tsType = "{ a : string; b : string }"
                        }
        , test "should decode requiredAt fields" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> requiredAt [ "a" ] string
                    |> requiredAt [ "b", "c" ] string
                    |> runWith """{"a":"foo","b":{"c":"bar"}}"""
                    |> Expect.equal
                        { decoded = Ok ( "foo", "bar" )
                        , tsType = "{ a : string; b : { c : string } }"
                        }
        , test "should decode optionalAt fields" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":"bar"}}"""
                    |> Expect.equal
                        { decoded = Ok ( "--", "bar" )
                        , tsType = "{ a? : { b? : string }; x? : { y? : string } }"
                        }
        , test "optional succeeds if the field is not present" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":"five"}"""
                    |> Expect.equal
                        { decoded = Ok ( "--", "five" )
                        , tsType = "{ a? : string; x? : string }"
                        }
        , test "optional succeeds with fallback if the field is present but null" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal
                        { decoded = Ok ( "--", "five" )
                        , tsType = "{ a? : string; x? : string }"
                        }
        , test "optional succeeds with result of the given decoder if the field is null and the decoder decodes nulls" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optional "a" (null "null") "--"
                    |> optional "x" string "--"
                    |> runWith """{"a":null,"x":"five"}"""
                    |> Expect.equal
                        { decoded = Ok ( "null", "five" )
                        , tsType = "{ a? : null; x? : string }"
                        }
        , test "optional fails if the field is present but doesn't decode" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optional "a" string "--"
                    |> optional "x" string "--"
                    |> runWith """{"x":5}"""
                    |> expectErr
        , test "optionalAt fails if the field is present but doesn't decode" <|
            \() ->
                TsDecode.succeed Tuple.pair
                    |> optionalAt [ "a", "b" ] string "--"
                    |> optionalAt [ "x", "y" ] string "--"
                    |> runWith """{"a":{},"x":{"y":5}}"""
                    |> expectErr

        --, test "resolveResult bubbles up decoded Err results" <|
        --    \() ->
        --        Decode.succeed Err
        --            |> required "error" string
        --            |> resolveResult
        --            |> runWith """{"error":"invalid"}"""
        --            |> expectErr
        --, test "resolveResult bubbles up decoded Ok results" <|
        --    \() ->
        --        Decode.succeed Ok
        --            |> required "ok" string
        --            |> resolveResult
        --            |> runWith """{"ok":"valid"}"""
        --            |> Expect.equal (Ok "valid")
        ]
