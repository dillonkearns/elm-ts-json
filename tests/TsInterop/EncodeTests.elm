module TsInterop.EncodeTests exposing (suite)

import Dict
import Expect exposing (Expectation)
import Json.Encode as Encode
import Test exposing (Test, describe, only, skip, test, todo)
import TsInterop.Encode as Encoder exposing (Encoder, required)


suite : Test
suite =
    describe "encode"
        [ test "object" <|
            \() ->
                Encoder.object
                    [ required "first" .first Encoder.string
                    , required "last" .last Encoder.string
                    ]
                    |> expectEncodes
                        { input = { first = "Dillon", last = "Kearns" }
                        , output = """{"first":"Dillon","last":"Kearns"}"""
                        , typeDef = "{ first : string; last : string }"
                        }
        , test "optional object" <|
            \() ->
                Encoder.object
                    [ required "first" .first Encoder.string
                    , Encoder.optional "middle" .middle Encoder.string
                    , required "last" .last Encoder.string
                    ]
                    |> expectEncodes
                        { input = { first = "Nyota", middle = Nothing, last = "Uhura" }
                        , output = """{"first":"Nyota","last":"Uhura"}"""
                        , typeDef = "{ first : string; middle? : string; last : string }"
                        }
        , test "standalone string" <|
            \() ->
                Encoder.string
                    |> Encoder.map .first
                    |> expectEncodes
                        { input = { first = "Dillon", last = "Kearns" }
                        , output = "\"Dillon\""
                        , typeDef = "string"
                        }
        , test "int" <|
            \() ->
                Encoder.int
                    |> expectEncodes
                        { input = 123
                        , output = "123"
                        , typeDef = "number"
                        }
        , test "float" <|
            \() ->
                Encoder.float
                    |> expectEncodes
                        { input = 123.45
                        , output = "123.45"
                        , typeDef = "number"
                        }
        , test "Encode.value escape hatch" <|
            \() ->
                Encoder.value
                    |> expectEncodes
                        { input = Encode.list Encode.string [ "Hello", "World" ]
                        , output = """["Hello","World"]"""
                        , typeDef = "JsonValue"
                        }
        , test "list" <|
            \() ->
                Encoder.list Encoder.string
                    |> expectEncodes
                        { input = [ "Item 1", "Item 2" ]
                        , output = "[\"Item 1\",\"Item 2\"]"
                        , typeDef = "string[]"
                        }
        , test "tuple" <|
            \() ->
                Encoder.tuple Encoder.string Encoder.int
                    |> expectEncodes
                        { input = ( "Item 1", 789 )
                        , output = "[\"Item 1\",789]"
                        , typeDef = "[ string, number ]"
                        }
        , test "triple" <|
            \() ->
                Encoder.triple Encoder.string Encoder.int Encoder.int
                    |> expectEncodes
                        { input = ( "Item 1", 123, 789 )
                        , output = "[\"Item 1\",123,789]"
                        , typeDef = "[ string, number, number ]"
                        }
        , test "list of lists" <|
            \() ->
                Encoder.list
                    (Encoder.list Encoder.string)
                    |> expectEncodes
                        { input = [ [ "Item 1", "Item 2" ], [] ]
                        , output = "[[\"Item 1\",\"Item 2\"],[]]"
                        , typeDef = "string[][]"
                        }
        , test "dict" <|
            \() ->
                Encoder.dict identity Encoder.string
                    |> expectEncodes
                        { input = Dict.fromList [ ( "a", "123" ), ( "b", "456" ) ]
                        , output = """{"a":"123","b":"456"}"""
                        , typeDef = "{ [key: string]: string }"
                        }
        , test "dict with unions" <|
            \() ->
                Encoder.dict identity
                    (Encoder.union
                        (\vInfo vWarning vError value ->
                            case value of
                                Info ->
                                    vInfo

                                Warning ->
                                    vWarning

                                Error ->
                                    vError
                        )
                        |> Encoder.variantLiteral (Encode.string "info")
                        |> Encoder.variantLiteral (Encode.string "warning")
                        |> Encoder.variantLiteral (Encode.string "error")
                        |> Encoder.buildUnion
                    )
                    |> expectEncodes
                        { input = Dict.fromList [ ( "a", Info ), ( "b", Warning ) ]
                        , output = """{"a":"info","b":"warning"}"""
                        , typeDef = """{ [key: string]: "error" | "warning" | "info" }"""
                        }
        , test "union type with one variant" <|
            \() ->
                Encoder.union
                    (\vOnlyVariant value ->
                        case value of
                            OnlyVariant ->
                                vOnlyVariant
                    )
                    |> Encoder.variant0 "OnlyVariant"
                    |> Encoder.buildUnion
                    |> expectEncodes
                        { input = OnlyVariant
                        , output = """{"tag":"OnlyVariant"}"""
                        , typeDef = """{ tag : "OnlyVariant" }"""
                        }
        , test "union type with two variants" <|
            \() ->
                Encoder.union
                    (\vSendHeartbeat vAlert value ->
                        case value of
                            SendPresenceHeartbeat ->
                                vSendHeartbeat

                            Alert string ->
                                vAlert string
                    )
                    |> Encoder.variant0 "SendPresenceHeartbeat"
                    |> Encoder.variantObject "Alert" [ required "message" identity Encoder.string ]
                    |> Encoder.buildUnion
                    |> expectEncodes
                        { input = Alert "Hello!"
                        , output = """{"tag":"Alert","message":"Hello!"}"""
                        , typeDef = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat" }"""
                        }
        , test "variant with encoders" <|
            \() ->
                Encoder.union
                    (\vAdmin vRegular vGuest value ->
                        case value of
                            Admin name id ->
                                vAdmin { name = name, id = id }

                            Regular name ->
                                vRegular { name = name }

                            Guest ->
                                vGuest ()
                    )
                    |> Encoder.variant
                        (Encoder.object
                            [ required "name" .name Encoder.string
                            , required "id" .id Encoder.int
                            ]
                        )
                    |> Encoder.variant
                        (Encoder.object
                            [ required "name" .name Encoder.string ]
                        )
                    |> Encoder.variant
                        (Encoder.object [])
                    |> Encoder.buildUnion
                    |> expectEncodes
                        { input = Admin "Dillon" 123
                        , output = """{"name":"Dillon","id":123}"""
                        , typeDef = """{  } | { name : string } | { name : string; id : number }"""
                        }
        , test "merge object to variant" <|
            \() ->
                Encoder.union
                    (\vSendHeartbeat vAlert value ->
                        case value of
                            SendPresenceHeartbeat ->
                                vSendHeartbeat

                            Alert string ->
                                vAlert string
                    )
                    |> Encoder.variant0 "SendPresenceHeartbeat"
                    |> Encoder.variantObject "Alert" [ required "message" identity Encoder.string ]
                    |> Encoder.buildUnion
                    |> expectEncodes
                        { input = Alert "Hello!"
                        , output = """{"tag":"Alert","message":"Hello!"}"""
                        , typeDef = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat" }"""
                        }
        , describe "unions"
            [ test "string literal" <|
                \() ->
                    Encoder.union
                        (\vInfo vWarning vError value ->
                            case value of
                                Info ->
                                    vInfo

                                Warning ->
                                    vWarning

                                Error ->
                                    vError
                        )
                        |> Encoder.variantLiteral (Encode.string "info")
                        |> Encoder.variantLiteral (Encode.string "warning")
                        |> Encoder.variantLiteral (Encode.string "error")
                        |> Encoder.buildUnion
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
        |> Encoder.encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \_ -> Encoder.typeDef interop |> Expect.equal expect.typeDef
            ]


expectEncodesNew :
    List ( input, String )
    -> String
    -> Encoder input
    -> Expectation
expectEncodesNew cases expectedTypeDef interop =
    ()
        |> Expect.all
            ((\() -> Encoder.typeDef interop |> Expect.equal expectedTypeDef)
                :: (cases
                        |> List.map
                            (\( input, expectedOutput ) ->
                                \() ->
                                    input
                                        |> Encoder.encoder interop
                                        |> Encode.encode 0
                                        |> Expect.equal expectedOutput
                            )
                   )
            )
