module TsPortTests exposing (..)

import Expect exposing (Expectation)
import Json.Encode as Encode
import Test exposing (..)
import TsInterop.Encode as Encoder exposing (Encoder, property)


suite : Test
suite =
    describe "Interop"
        [ describe "encode"
            [ test "object" <|
                \() ->
                    Encoder.build
                        |> property "first" (Encoder.string |> Encoder.map .first)
                        |> property "last" (Encoder.string |> Encoder.map .last)
                        |> Encoder.toEncoder
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = """{"last":"Kearns","first":"Dillon"}"""
                            , typeDef = "{ last : string; first : string }"
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
            , test "list" <|
                \() ->
                    Encoder.list Encoder.string
                        |> expectEncodes
                            { input = [ "Item 1", "Item 2" ]
                            , output = "[\"Item 1\",\"Item 2\"]"
                            , typeDef = "string[]"
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
            , test "custom type with one variant" <|
                \() ->
                    Encoder.custom
                        (\vOnlyVariant value ->
                            case value of
                                OnlyVariant ->
                                    vOnlyVariant
                        )
                        |> Encoder.variant0 "OnlyVariant"
                        |> Encoder.buildCustom
                        |> expectEncodes
                            { input = OnlyVariant
                            , output = """{"tag":"OnlyVariant"}"""
                            , typeDef = """{ tag : "OnlyVariant" }"""
                            }
            , test "custom type with two variants" <|
                \() ->
                    Encoder.custom
                        (\vSendHeartbeat vAlert value ->
                            case value of
                                SendPresenceHeartbeat ->
                                    vSendHeartbeat

                                Alert string ->
                                    vAlert string
                        )
                        |> Encoder.variant0 "SendPresenceHeartbeat"
                        |> Encoder.objectVariant "Alert"
                            (Encoder.build
                                |> property "message" Encoder.string
                            )
                        |> Encoder.buildCustom
                        |> expectEncodes
                            { input = Alert "Hello!"
                            , output = """{"tag":"Alert","message":"Hello!"}"""
                            , typeDef = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat" }"""
                            }
            , test "merge object to variant" <|
                \() ->
                    Encoder.custom
                        (\vSendHeartbeat vAlert value ->
                            case value of
                                SendPresenceHeartbeat ->
                                    vSendHeartbeat

                                Alert string ->
                                    vAlert string
                        )
                        |> Encoder.variant0 "SendPresenceHeartbeat"
                        |> Encoder.objectVariant "Alert"
                            (Encoder.build
                                |> property "message" Encoder.string
                            )
                        |> Encoder.buildCustom
                        |> expectEncodes
                            { input = Alert "Hello!"
                            , output = """{"tag":"Alert","message":"Hello!"}"""
                            , typeDef = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat" }"""
                            }
            , describe "unions"
                [ test "string literal" <|
                    \() ->
                        Encoder.custom
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
                            |> Encoder.buildCustom
                            |> expectEncodes
                                { input = Warning
                                , output = "\"warning\""
                                , typeDef = "\"error\" | \"warning\" | \"info\""
                                }
                ]
            ]
        ]


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
    { output : String, input : encodesFrom, typeDef : String }
    -> Encoder encodesFrom
    -> Expect.Expectation
expectEncodes expect interop =
    expect.input
        |> Encoder.encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \decoded -> Encoder.typeDef interop |> Expect.equal expect.typeDef
            ]
