module TsJson.EncodeTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Json.Encode as Encode
import Test exposing (Test, describe, only, skip, test, todo)
import TsJson.Encode as TsEncode exposing (Encoder, required)
import TsType


suite : Test
suite =
    describe "encode"
        [ test "object" <|
            \() ->
                TsEncode.object
                    [ required "first" .first TsEncode.string
                    , required "last" .last TsEncode.string
                    ]
                    |> expectEncodes
                        { input = { first = "Dillon", last = "Kearns" }
                        , output = """{"first":"Dillon","last":"Kearns"}"""
                        , typeDef = "{ first : string; last : string }"
                        }
        , test "optional object" <|
            \() ->
                TsEncode.object
                    [ required "first" .first TsEncode.string
                    , TsEncode.optional "middle" .middle TsEncode.string
                    , required "last" .last TsEncode.string
                    ]
                    |> expectEncodes
                        { input = { first = "Nyota", middle = Nothing, last = "Uhura" }
                        , output = """{"first":"Nyota","last":"Uhura"}"""
                        , typeDef = "{ first : string; last : string; middle? : string }"
                        }
        , test "standalone string" <|
            \() ->
                TsEncode.string
                    |> TsEncode.map .first
                    |> expectEncodes
                        { input = { first = "Dillon", last = "Kearns" }
                        , output = "\"Dillon\""
                        , typeDef = "string"
                        }
        , test "int" <|
            \() ->
                TsEncode.int
                    |> expectEncodes
                        { input = 123
                        , output = "123"
                        , typeDef = "number"
                        }
        , test "float" <|
            \() ->
                TsEncode.float
                    |> expectEncodes
                        { input = 123.45
                        , output = "123.45"
                        , typeDef = "number"
                        }
        , test "Encode.value escape hatch" <|
            \() ->
                TsEncode.value
                    |> expectEncodes
                        { input = Encode.list Encode.string [ "Hello", "World" ]
                        , output = """["Hello","World"]"""
                        , typeDef = "JsonValue"
                        }
        , test "list" <|
            \() ->
                TsEncode.list TsEncode.string
                    |> expectEncodes
                        { input = [ "Item 1", "Item 2" ]
                        , output = "[\"Item 1\",\"Item 2\"]"
                        , typeDef = "string[]"
                        }
        , test "tuple" <|
            \() ->
                TsEncode.tuple TsEncode.string TsEncode.int
                    |> expectEncodes
                        { input = ( "Item 1", 789 )
                        , output = "[\"Item 1\",789]"
                        , typeDef = "[ string, number ]"
                        }
        , test "triple" <|
            \() ->
                TsEncode.triple TsEncode.string TsEncode.int TsEncode.int
                    |> expectEncodes
                        { input = ( "Item 1", 123, 789 )
                        , output = "[\"Item 1\",123,789]"
                        , typeDef = "[ string, number, number ]"
                        }
        , test "list of lists" <|
            \() ->
                TsEncode.list
                    (TsEncode.list TsEncode.string)
                    |> expectEncodes
                        { input = [ [ "Item 1", "Item 2" ], [] ]
                        , output = "[[\"Item 1\",\"Item 2\"],[]]"
                        , typeDef = "string[][]"
                        }
        , test "dict" <|
            \() ->
                TsEncode.dict identity TsEncode.string
                    |> expectEncodes
                        { input = Dict.fromList [ ( "a", "123" ), ( "b", "456" ) ]
                        , output = """{"a":"123","b":"456"}"""
                        , typeDef = "{ [key: string]: string }"
                        }
        , test "dict with unions" <|
            \() ->
                TsEncode.dict identity
                    (TsEncode.union
                        (\vInfo vWarning vError value ->
                            case value of
                                Info ->
                                    vInfo

                                Warning ->
                                    vWarning

                                Error ->
                                    vError
                        )
                        |> TsEncode.variantLiteral (Encode.string "info")
                        |> TsEncode.variantLiteral (Encode.string "warning")
                        |> TsEncode.variantLiteral (Encode.string "error")
                        |> TsEncode.buildUnion
                    )
                    |> expectEncodes
                        { input = Dict.fromList [ ( "a", Info ), ( "b", Warning ) ]
                        , output = """{"a":"info","b":"warning"}"""
                        , typeDef = """{ [key: string]: "error" | "warning" | "info" }"""
                        }
        , test "union type with one variant" <|
            \() ->
                TsEncode.union
                    (\vOnlyVariant value ->
                        case value of
                            OnlyVariant ->
                                vOnlyVariant
                    )
                    |> TsEncode.variant0 "OnlyVariant"
                    |> TsEncode.buildUnion
                    |> expectEncodes
                        { input = OnlyVariant
                        , output = """{"tag":"OnlyVariant"}"""
                        , typeDef = """{ tag : "OnlyVariant" }"""
                        }
        , test "union type with two variants" <|
            \() ->
                TsEncode.union
                    (\vSendHeartbeat vAlert value ->
                        case value of
                            SendPresenceHeartbeat ->
                                vSendHeartbeat

                            Alert string ->
                                vAlert string
                    )
                    |> TsEncode.variant0 "SendPresenceHeartbeat"
                    |> TsEncode.variantObject "Alert" [ required "message" identity TsEncode.string ]
                    |> TsEncode.buildUnion
                    |> expectEncodes
                        { input = Alert "Hello!"
                        , output = """{"tag":"Alert","message":"Hello!"}"""
                        , typeDef = """{ message : string; tag : "Alert" } | { tag : "SendPresenceHeartbeat" }"""
                        }
        , test "variant with encoders" <|
            \() ->
                TsEncode.union
                    (\vAdmin vRegular vGuest value ->
                        case value of
                            Admin name id ->
                                vAdmin { name = name, id = id }

                            Regular name ->
                                vRegular { name = name }

                            Guest ->
                                vGuest ()
                    )
                    |> TsEncode.variant
                        (TsEncode.object
                            [ required "name" .name TsEncode.string
                            , required "id" .id TsEncode.int
                            ]
                        )
                    |> TsEncode.variant
                        (TsEncode.object
                            [ required "name" .name TsEncode.string ]
                        )
                    |> TsEncode.variant
                        (TsEncode.object [])
                    |> TsEncode.buildUnion
                    |> expectEncodes
                        { input = Admin "Dillon" 123
                        , output = """{"name":"Dillon","id":123}"""
                        , typeDef = """{  } | { name : string } | { id : number; name : string }"""
                        }
        , test "merge object to variant" <|
            \() ->
                TsEncode.union
                    (\vSendHeartbeat vAlert value ->
                        case value of
                            SendPresenceHeartbeat ->
                                vSendHeartbeat

                            Alert string ->
                                vAlert string
                    )
                    |> TsEncode.variant0 "SendPresenceHeartbeat"
                    |> TsEncode.variantObject "Alert" [ required "message" identity TsEncode.string ]
                    |> TsEncode.buildUnion
                    |> expectEncodes
                        { input = Alert "Hello!"
                        , output = """{"tag":"Alert","message":"Hello!"}"""
                        , typeDef = """{ message : string; tag : "Alert" } | { tag : "SendPresenceHeartbeat" }"""
                        }
        , describe "unions"
            [ test "string literal" <|
                \() ->
                    TsEncode.union
                        (\vInfo vWarning vError value ->
                            case value of
                                Info ->
                                    vInfo

                                Warning ->
                                    vWarning

                                Error ->
                                    vError
                        )
                        |> TsEncode.variantLiteral (Encode.string "info")
                        |> TsEncode.variantLiteral (Encode.string "warning")
                        |> TsEncode.variantLiteral (Encode.string "error")
                        |> TsEncode.buildUnion
                        |> expectEncodes
                            { input = Warning
                            , output = "\"warning\""
                            , typeDef = "\"error\" | \"warning\" | \"info\""
                            }
            ]
        ]


type User
    = Admin String Int
    | Regular String
    | Guest


type Severity
    = Info
    | Warning
    | Error


type WithOneVariant
    = OnlyVariant


type ToJs
    = SendPresenceHeartbeat
    | Alert String


expectEncodes :
    { output : String, input : input, typeDef : String }
    -> Encoder input
    -> Expectation
expectEncodes expect interop =
    expect.input
        |> TsEncode.encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \_ -> encoderType interop |> Expect.equal expect.typeDef
            ]


expectEncodesNew :
    List ( input, String )
    -> String
    -> Encoder input
    -> Expectation
expectEncodesNew cases expectedTypeDef interop =
    ()
        |> Expect.all
            ((\() -> encoderType interop |> Expect.equal expectedTypeDef)
                :: (cases
                        |> List.map
                            (\( input, expectedOutput ) ->
                                \() ->
                                    input
                                        |> TsEncode.encoder interop
                                        |> Encode.encode 0
                                        |> Expect.equal expectedOutput
                            )
                   )
            )


encoderType : Encoder input -> String
encoderType encoder_ =
    encoder_
        |> TsEncode.tsType
        |> TsType.toString
