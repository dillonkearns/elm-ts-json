module TsInterop.DecodeTests exposing (..)

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
        , test "int" <|
            \() ->
                int
                    |> expectDecodes
                        { input = "123"
                        , output = 123
                        , typeDef = "number"
                        }
        , test "float" <|
            \() ->
                float
                    |> expectDecodes
                        { input = "123.45"
                        , output = 123.45
                        , typeDef = "number"
                        }
        , test "bool" <|
            \() ->
                bool
                    |> expectDecodes
                        { input = "true"
                        , output = True
                        , typeDef = "boolean"
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
        , describe "objects"
            [ test "single field" <|
                \() ->
                    field "field" bool
                        |> expectDecodes
                            { input = """{"field": true}"""
                            , output = True
                            , typeDef = "{ field : boolean }"
                            }
            , test "multiple fields" <|
                \() ->
                    map2 Tuple.pair
                        (field "field1" bool)
                        (field "field2" bool)
                        |> expectDecodes
                            { input = """{"field1": true, "field2": false}"""
                            , output = ( True, False )
                            , typeDef = "{ field1 : boolean; field2 : boolean }"
                            }
            ]
        ]


type Severity
    = Info
    | Warning
    | Error


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
