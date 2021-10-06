module CodecAdvancedTests exposing (Semaphore(..), roundtripTest, suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, test)
import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode
import TsJson.Encode
import TsType


type Semaphore
    = Red Int String
    | Yellow
    | Green Float


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    Codec.custom (Just "color")
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f ->
                    green f
        )
        |> Codec.namedVariant2 "red" Red ( "first", Codec.int ) ( "second", Codec.string )
        |> Codec.variant0 "yellow" Yellow
        |> Codec.namedVariant1 "green" Green ( "value", Codec.float )
        |> Codec.buildCustom


type Shape
    = Rectangle Int Int
    | Square Int
    | Circle Int


shapeCodec : Codec.Codec Shape
shapeCodec =
    Codec.custom (Just "shape")
        (\rectangle square circle value ->
            case value of
                Rectangle width height ->
                    rectangle width height

                Square width ->
                    square width

                Circle radius ->
                    circle radius
        )
        |> Codec.namedVariant2 "rectangle" Rectangle ( "width", Codec.int ) ( "height", Codec.int )
        |> Codec.namedVariant1 "square" Square ( "width", Codec.int )
        |> Codec.namedVariant1 "circle" Circle ( "radius", Codec.int )
        |> Codec.buildCustom


suite : Test
suite =
    describe "Testing customObjectCodec"
        [ describe "Roundtrip" [ roundtripTest ]
        , describe "Correct shapes" shapesTests
        , test "TsType" <|
            \() ->
                semaphoreCodec
                    |> Codec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ color : "green"; value : number } | { color : "yellow" } | { color : "red"; first : number; second : string }"""
        , test "shape TsType" <|
            \() ->
                shapeCodec
                    |> Codec.tsType
                    |> TsType.toString
                    |> Expect.equal
                        """{ radius : number; shape : "circle" } | { shape : "square"; width : number } | { height : number; shape : "rectangle"; width : number }"""
        ]


roundtripTest : Test
roundtripTest =
    fuzz semaphoreFuzzer "is a roundtrip" <|
        \value ->
            value
                |> TsJson.Encode.encoder (Codec.encoder semaphoreCodec)
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
      , decodeString (Codec.fail "Expecting an OBJECT with a field named `value`") """{"color": "green"}"""
      )
    , ( "Wrong tag fail"
      , [ ( "color", JE.string "gray" ) ]
      , decodeString (Codec.fail "color \"gray\" did not match") """{"color": "gray"}"""
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
    Codec.decoder codec
        |> TsJson.Decode.decoder
        |> Json.Decode.decodeValue


decodeString : Codec a -> String -> Result Json.Decode.Error a
decodeString codec =
    Codec.decoder codec
        |> TsJson.Decode.decoder
        |> Json.Decode.decodeString
