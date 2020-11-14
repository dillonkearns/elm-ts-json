module TsInteropDecodeTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Interop decode"
        [ --test "object" <|
          --    \() ->
          --        TsPort.build
          --            |> property "first" (TsPort.string |> TsPort.map .first)
          --            |> property "last" (TsPort.string |> TsPort.map .last)
          --            |> TsPort.toEncoder
          --            |> expectEncodes
          --                { input = { first = "Dillon", last = "Kearns" }
          --                , output = """{"last":"Kearns","first":"Dillon"}"""
          --                , typeDef = "{ last : string; first : string }"
          --                }
          test "standalone string" <|
            \() ->
                string
                    |> expectDecodes
                        { input = "\"Dillon\""
                        , output = "Dillon"
                        , typeDef = "string"
                        }
        , test "list of strings" <|
            \() ->
                list string
                    |> expectDecodes
                        { input = """["Hello", "World"]"""
                        , output = [ "Hello", "World" ]
                        , typeDef = "string[]"
                        }
        , test "literal" <|
            \() ->
                literal "Hello" (Encode.list Encode.string [ "Hello" ])
                    |> expectDecodes
                        { input = "[\"Hello\"]"
                        , output = "Hello"
                        , typeDef = "[\"Hello\"]"
                        }
        , test "literals" <|
            \() ->
                literal "Hello" (Encode.list Encode.string [ "Hello" ])
                    |> expectDecodes
                        { input = "[\"Hello\"]"
                        , output = "Hello"
                        , typeDef = "[\"Hello\"]"
                        }
        ]


type TsType
    = String
    | List TsType
    | Literal Encode.Value


type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


literal : value -> Encode.Value -> InteropDecoder value
literal value literalValue =
    InteropDecoder
        (Decode.value
            |> Decode.andThen
                (\decodeValue ->
                    if literalValue == decodeValue then
                        Decode.succeed value

                    else
                        Decode.fail ("Expected the following literal value: " ++ Encode.encode 0 literalValue)
                )
        )
        (Literal literalValue)


string : InteropDecoder String
string =
    InteropDecoder Decode.string String


list : InteropDecoder value -> InteropDecoder (List value)
list (InteropDecoder innerDecoder innerType) =
    --InteropDecoder Decode.string String
    InteropDecoder (Decode.list innerDecoder) (List innerType)


decoder : InteropDecoder value -> Decoder value
decoder (InteropDecoder decoder_ tsType_) =
    decoder_


tsTypeToString : InteropDecoder value -> String
tsTypeToString (InteropDecoder decoder_ tsType_) =
    tsTypeToString_ tsType_


tsTypeToString_ : TsType -> String
tsTypeToString_ tsType_ =
    case tsType_ of
        String ->
            "string"

        List listType ->
            tsTypeToString_ listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue


expectDecodes :
    { output : decodesTo, input : String, typeDef : String }
    -> InteropDecoder decodesTo
    -> Expect.Expectation
expectDecodes expect interop =
    expect.input
        |> Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \decoded -> tsTypeToString interop |> Expect.equal expect.typeDef
            ]
