module TsPortTests exposing (..)

import Expect exposing (Expectation)
import Json.Encode as Encode
import Test exposing (..)
import TsPort exposing (Encoder, property)


suite : Test
suite =
    describe "Interop"
        [ describe "encode"
            [ test "object" <|
                \() ->
                    TsPort.build
                        |> property "first" (TsPort.string |> TsPort.map .first)
                        |> property "last" (TsPort.string |> TsPort.map .last)
                        |> TsPort.toEncoder
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = """{"last":"Kearns","first":"Dillon"}"""
                            , typeDef = "{ last : string; first : string }"
                            }
            , test "standalone string" <|
                \() ->
                    TsPort.string
                        |> TsPort.map .first
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = "\"Dillon\""
                            , typeDef = "string"
                            }
            , test "list" <|
                \() ->
                    TsPort.list TsPort.string
                        |> expectEncodes
                            { input = [ "Item 1", "Item 2" ]
                            , output = "[\"Item 1\",\"Item 2\"]"
                            , typeDef = "string[]"
                            }
            , test "list of lists" <|
                \() ->
                    TsPort.list
                        (TsPort.list TsPort.string)
                        |> expectEncodes
                            { input = [ [ "Item 1", "Item 2" ], [] ]
                            , output = "[[\"Item 1\",\"Item 2\"],[]]"
                            , typeDef = "string[][]"
                            }
            , test "custom type with one variant" <|
                \() ->
                    TsPort.custom
                        (\vOnlyVariant value ->
                            case value of
                                OnlyVariant ->
                                    vOnlyVariant
                        )
                        |> TsPort.variant0 "OnlyVariant"
                        |> TsPort.buildCustom
                        |> expectEncodes
                            { input = OnlyVariant
                            , output = """{"tag":"OnlyVariant"}"""
                            , typeDef = """{ tag : "OnlyVariant";  }"""
                            }
            , test "custom type with two variants" <|
                \() ->
                    TsPort.custom
                        (\vSendHeartbeat vAlert value ->
                            case value of
                                SendPresenceHeartbeat ->
                                    vSendHeartbeat

                                Alert string ->
                                    vAlert string
                        )
                        |> TsPort.variant0 "SendPresenceHeartbeat"
                        |> TsPort.variant1 "Alert" TsPort.string
                        |> TsPort.buildCustom
                        |> expectEncodes
                            { input = Alert "Hello!"
                            , output = """{"tag":"Alert","args":["Hello!"]}"""
                            , typeDef = """{ tag : "Alert"; args: [ string ]; } | { tag : "SendPresenceHeartbeat";  }"""
                            }
            , test "merge object to variant" <|
                \() ->
                    TsPort.custom
                        (\vSendHeartbeat vAlert value ->
                            case value of
                                SendPresenceHeartbeat ->
                                    vSendHeartbeat

                                Alert string ->
                                    vAlert string
                        )
                        |> TsPort.variant0 "SendPresenceHeartbeat"
                        |> TsPort.objectVariant "Alert"
                            (TsPort.build
                                |> property "message" TsPort.string
                            )
                        |> TsPort.buildCustom
                        |> expectEncodes
                            { input = Alert "Hello!"
                            , output = """{"tag":"Alert","message":"Hello!"}"""
                            , typeDef = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat";  }"""
                            }
            , describe "unions"
                [ test "string literals" <|
                    \() ->
                        TsPort.custom
                            (\vInfo vWarning vError value ->
                                case value of
                                    Info ->
                                        vInfo

                                    Warning ->
                                        vWarning

                                    Error ->
                                        vError
                            )
                            |> TsPort.variantLiteral (Encode.string "info")
                            |> TsPort.variantLiteral (Encode.string "warning")
                            |> TsPort.variantLiteral (Encode.string "error")
                            |> TsPort.buildCustom
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
        |> TsPort.encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \decoded -> TsPort.typeDef interop |> Expect.equal expect.typeDef
            ]
