module TsJson.DecodeTests exposing (suite)

import Array
import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import TsJson.Decode exposing (..)
import TsType


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
        , test "array of strings" <|
            \() ->
                array string
                    |> expectDecodes
                        { input = """["Hello", "World"]"""
                        , output = Array.fromList [ "Hello", "World" ]
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
        , test "andThen" <|
            \() ->
                let
                    example =
                        andThenInit
                            (\v1Decoder v2PlusDecoder version ->
                                case version of
                                    1 ->
                                        v1Decoder

                                    _ ->
                                        v2PlusDecoder
                            )
                            |> andThenDecoder (field "payload" string)
                            |> andThenDecoder (at [ "data", "payload" ] string)
                in
                field "version" int
                    |> andThen example
                    |> expectDecodes
                        { input = """{"version": 1, "payload": "Hello"}"""
                        , output = "Hello"
                        , typeDef = "({ version : number } & ({ data : { payload : string } } | { payload : string }))"
                        }
        , test "succeed" <|
            \() ->
                succeed "Hello"
                    |> expectDecodes
                        { input = "null"
                        , output = "Hello"
                        , typeDef = "JsonValue"
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
            , test "combining contradictory types" <|
                \() ->
                    map2 Tuple.pair
                        (field "field1" bool)
                        int
                        |> expectDecodeError
                            { input = """{"field1": true, "field2": false}"""
                            , typeDef = "never"
                            }
            , test "object with literal values" <|
                \() ->
                    succeed (\() name -> { name = name })
                        |> andMap (field "kind" (literal () (Encode.string "regular")))
                        |> andMap (field "name" string)
                        |> expectDecodes
                            { input = """{"kind": "regular", "name": "Leonard"}"""
                            , typeDef = """{ kind : "regular"; name : string }"""
                            , output = { name = "Leonard" }
                            }
            ]
        , test "old discriminated union style" <|
            \() ->
                oneOf
                    [ succeed (\() id -> Admin { id = id })
                        |> andMap (field "role" (literal () (Encode.string "admin")))
                        |> andMap (field "id" int)
                    , literal Guest (Encode.object [ ( "role", Encode.string "guest" ) ])
                    ]
                    |> expectDecodes
                        { input = """{"role": "admin", "id": 123}"""
                        , typeDef = """{ id : number; role : "admin" } | {"role":"guest"}"""
                        , output = Admin { id = 123 }
                        }
        , test "discriminatedUnion" <|
            \() ->
                discriminatedUnion "role"
                    [ ( "admin"
                      , succeed (\id -> Admin { id = id })
                            |> andMap (field "id" int)
                      )
                    , ( "guest", succeed Guest )
                    ]
                    |> expectDecodesList
                        { examples =
                            [ ( """{"role": "admin", "id": 123}"""
                              , Ok (Admin { id = 123 })
                              )
                            , ( """{"role": "unexpected", "id": 123}"""
                              , Err """Problem with the given value:

{
        "role": "unexpected",
        "id": 123
    }

Unexpected discriminant value 'unexpected' for field 'role'"""
                              )
                            ]
                        , typeDef = """{ id : number; role : "admin" } | { role : "guest" }"""
                        }
        , test "stringUnion" <|
            \() ->
                stringUnion
                    [ ( "info", Info )
                    , ( "warning", Warning )
                    , ( "error", Error )
                    ]
                    |> expectDecodesList
                        { examples =
                            [ ( "\"info\""
                              , Ok Info
                              )
                            , ( "\"unexpected-string\""
                              , Err """Problem with the given value:

"unexpected-string"

I was expecting a string union with one of these string values: [ "info", "warning", "error" ]"""
                              )
                            ]
                        , typeDef = "\"info\" | \"warning\" | \"error\""
                        }
        , test "stringLiteral" <|
            \() ->
                stringLiteral () "HELLO!"
                    |> expectDecodesList
                        { examples =
                            [ ( "\"HELLO!\""
                              , Ok ()
                              )
                            , ( "\"unexpected-string\""
                              , Err """Problem with the given value:

"unexpected-string"

Expected the following string literal value: "HELLO!\""""
                              )
                            ]
                        , typeDef = "\"HELLO!\""
                        }
        ]


type User
    = Admin { id : Int }
    | Guest


type Severity
    = Info
    | Warning
    | Error


expectDecodes :
    { output : decodesTo, input : String, typeDef : String }
    -> Decoder decodesTo
    -> Expectation
expectDecodes expect interop =
    expect.input
        |> Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \_ -> tsTypeToString interop |> Expect.equal expect.typeDef
            ]


expectDecodesList :
    { examples : List ( String, Result String decodesTo ), typeDef : String }
    -> Decoder decodesTo
    -> Expectation
expectDecodesList expect interop =
    expect.examples
        |> List.map
            (\( inputString, expected ) ->
                ( Decode.decodeString (decoder interop) inputString |> Result.mapError Decode.errorToString, expected )
            )
        |> Expect.all
            [ \decoded ->
                Expect.equalLists
                    (decoded |> List.map Tuple.first)
                    (decoded |> List.map Tuple.second)
            , \_ -> tsTypeToString interop |> Expect.equal expect.typeDef
            ]


expectDecodeError :
    { input : String, typeDef : String }
    -> Decoder decodesTo
    -> Expectation
expectDecodeError expect interop =
    expect.input
        |> Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded ->
                case decoded of
                    Err error ->
                        Expect.pass

                    Ok value ->
                        Expect.fail <| "Expected decode failure, but got " ++ Debug.toString value
            , \_ -> tsTypeToString interop |> Expect.equal expect.typeDef
            ]


tsTypeToString : Decoder value -> String
tsTypeToString tsDecoder =
    tsDecoder |> tsType |> TsType.toString
