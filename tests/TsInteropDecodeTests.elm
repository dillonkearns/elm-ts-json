module TsInteropDecodeTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (..)
import TsInterop.Decode exposing (..)


suite : Test
suite =
    describe "Interop decode"
        [ test "standalone string" <|
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
                oneOf
                    [ literal Info (Encode.string "info")
                    , literal Warning (Encode.string "warning")
                    , literal Error (Encode.string "error")
                    ]
                    |> expectDecodes
                        { input = "\"info\""
                        , output = Info
                        , typeDef = "\"info\" | \"warning\" | \"error\""
                        }
        , test "nullable" <|
            \() ->
                nullable string
                    |> expectDecodes
                        { input = "null"
                        , output = Nothing
                        , typeDef = "string | null"
                        }
        , test "map" <|
            \() ->
                string
                    |> map String.toInt
                    |> expectDecodes
                        { input = "\"123\""
                        , output = Just 123
                        , typeDef = "string"
                        }
        ]


type Severity
    = Info
    | Warning
    | Error


type TsType
    = String
    | List TsType
    | Literal Encode.Value
    | Union (List TsType)
    | Null


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
