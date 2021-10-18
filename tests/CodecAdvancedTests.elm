module CodecAdvancedTests exposing (Semaphore(..), roundtripTest, suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, fuzz, test)
import TsJson.Codec as TsCodec exposing (Codec)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import TsType


type Semaphore
    = Red Int String
    | Yellow
    | Green Float


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    TsCodec.custom (Just "color")
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f ->
                    green f
        )
        |> TsCodec.namedVariant2 "red" Red ( "first", TsCodec.int ) ( "second", TsCodec.string )
        |> TsCodec.variant0 "yellow" Yellow
        |> TsCodec.namedVariant1 "green" Green ( "value", TsCodec.float )
        |> TsCodec.buildCustom


type Shape
    = Rectangle Int Int
    | Square Int
    | Circle Int


shapeCodec : TsCodec.Codec Shape
shapeCodec =
    TsCodec.custom (Just "shape")
        (\rectangle square circle value ->
            case value of
                Rectangle width height ->
                    rectangle width height

                Square width ->
                    square width

                Circle radius ->
                    circle radius
        )
        |> TsCodec.namedVariant2 "rectangle" Rectangle ( "width", TsCodec.int ) ( "height", TsCodec.int )
        |> TsCodec.namedVariant1 "square" Square ( "width", TsCodec.int )
        |> TsCodec.namedVariant1 "circle" Circle ( "radius", TsCodec.int )
        |> TsCodec.buildCustom


suite : Test
suite =
    describe "Testing customObjectCodec"
        [ describe "Roundtrip" [ roundtripTest ]
        , describe "Correct shapes" shapesTests
        , test "TsType" <|
            \() ->
                semaphoreCodec
                    |> TsCodec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ color : "green"; value : number } | { color : "yellow" } | { color : "red"; first : number; second : string }"""
        , test "shape TsType" <|
            \() ->
                shapeCodec
                    |> TsCodec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ radius : number; shape : "circle" } | { shape : "square"; width : number } | { height : number; shape : "rectangle"; width : number }"""
        ]


roundtripTest : Test
roundtripTest =
    fuzz semaphoreFuzzer "is a roundtrip" <|
        \value ->
            value
                |> TsEncode.encoder (TsCodec.encoder semaphoreCodec)
                |> decodeValue semaphoreCodec
                |> Expect.equal (Ok value)


semaphoreFuzzer : Fuzzer Semaphore
semaphoreFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Red Fuzz.int Fuzz.string
        , Fuzz.constant Yellow
        , Fuzz.map Green Fuzz.float
        ]


shapesTests : List Test
shapesTests =
    [ ( "Red decode"
      , [ ( "color", Encode.string "red" )
        , ( "first", Encode.int 42 )
        , ( "second", Encode.string "413" )
        ]
      , Ok <| Red 42 "413"
      )
    , ( "Yellow decode"
      , [ ( "color", Encode.string "yellow" )
        , ( "extra", Encode.null )
        ]
      , Ok Yellow
      )
    , ( "Green decode"
      , [ ( "color", Encode.string "green" )
        , ( "value", Encode.float -42 )
        ]
      , Ok <| Green -42
      )
    , ( "Missing fields fail"
      , [ ( "color", Encode.string "green" ) ]
      , decodeString (TsCodec.fail "Expecting an OBJECT with a field named `value`") """{"color": "green"}"""
      )
    , ( "Wrong tag fail"
      , [ ( "color", Encode.string "gray" ) ]
      , decodeString (TsCodec.fail "color \"gray\" did not match") """{"color": "gray"}"""
      )
    ]
        |> List.map
            (\( name, fields, expected ) ->
                Test.test name <|
                    \_ ->
                        fields
                            |> Encode.object
                            |> decodeValue semaphoreCodec
                            |> Expect.equal expected
            )


decodeValue : Codec a -> Decode.Value -> Result Decode.Error a
decodeValue codec =
    TsCodec.decoder codec
        |> TsDecode.decoder
        |> Decode.decodeValue


decodeString : Codec a -> String -> Result Decode.Error a
decodeString codec =
    TsCodec.decoder codec
        |> TsDecode.decoder
        |> Decode.decodeString
