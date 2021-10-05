module CodecAdvancedTests exposing (Semaphore(..), roundtripTest, suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, test)
import TsJson.Codec exposing (Codec)
import TsJson.Codec.Advanced as Codec
import TsJson.Decode
import TsJson.Encode
import TsType


type Semaphore
    = Red Int String
    | Yellow
    | Green Float


semaphoreCodec : TsJson.Codec.Codec Semaphore
semaphoreCodec =
    Codec.customObject "color"
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f ->
                    green f
        )
        |> Codec.objectVariant2 "red" Red ( "first", TsJson.Codec.int ) ( "second", TsJson.Codec.string )
        |> Codec.objectVariant0 "yellow" Yellow
        |> Codec.objectVariant1 "green" Green ( "value", TsJson.Codec.float )
        |> Codec.buildCustomObject


type Shape
    = Rectangle Int Int
    | Square Int
    | Circle Int


shapeCodec : TsJson.Codec.Codec Shape
shapeCodec =
    Codec.customObject "shape"
        (\rectangle square circle value ->
            case value of
                Rectangle width height ->
                    rectangle width height

                Square width ->
                    square width

                Circle radius ->
                    circle radius
        )
        |> Codec.objectVariant2 "rectangle" Rectangle ( "width", TsJson.Codec.int ) ( "height", TsJson.Codec.int )
        |> Codec.objectVariant1 "square" Square ( "width", TsJson.Codec.int )
        |> Codec.objectVariant1 "circle" Circle ( "radius", TsJson.Codec.int )
        |> Codec.buildCustomObject


suite : Test
suite =
    describe "Testing customObjectCodec"
        [ describe "Roundtrip" [ roundtripTest ]
        , describe "Correct shapes" shapesTests
        , test "TsType" <|
            \() ->
                semaphoreCodec
                    |> TsJson.Codec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ color : "green"; value : number } | { color : "yellow" } | { color : "red"; first : number; second : string }"""
        , test "shape TsType" <|
            \() ->
                shapeCodec
                    |> TsJson.Codec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ radius : number; shape : "circle" } | { shape : "square"; width : number } | { height : number; shape : "rectangle"; width : number }"""
        ]


roundtripTest : Test
roundtripTest =
    fuzz semaphoreFuzzer "is a roundtrip" <|
        \value ->
            value
                |> TsJson.Encode.encoder (TsJson.Codec.encoder semaphoreCodec)
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
      , [ ( "color", JE.string "red" )
        , ( "first", JE.int 42 )
        , ( "second", JE.string "413" )
        ]
      , Ok <| Red 42 "413"
      )
    , ( "Yellow decode"
      , [ ( "color", JE.string "yellow" )
        , ( "extra", JE.null )
        ]
      , Ok Yellow
      )
    , ( "Green decode"
      , [ ( "color", JE.string "green" )
        , ( "value", JE.float -42 )
        ]
      , Ok <| Green -42
      )
    , ( "Missing fields fail"
      , [ ( "color", JE.string "green" ) ]
      , decodeString (TsJson.Codec.fail "Expecting an OBJECT with a field named `value`") """{"color": "green"}"""
      )
    , ( "Wrong tag fail"
      , [ ( "color", JE.string "gray" ) ]
      , decodeString (TsJson.Codec.fail "color \"gray\" did not match") """{"color": "gray"}"""
      )
    ]
        |> List.map
            (\( name, fields, expected ) ->
                Test.test name <|
                    \_ ->
                        fields
                            |> JE.object
                            |> decodeValue semaphoreCodec
                            |> Expect.equal expected
            )


decodeValue : Codec a -> Json.Decode.Value -> Result Json.Decode.Error a
decodeValue codec =
    TsJson.Codec.decoder codec
        |> TsJson.Decode.decoder
        |> Json.Decode.decodeValue


decodeString : Codec a -> String -> Result Json.Decode.Error a
decodeString codec =
    TsJson.Codec.decoder codec
        |> TsJson.Decode.decoder
        |> Json.Decode.decodeString
