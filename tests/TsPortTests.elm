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
                            , output = """{"type":"OnlyVariant"}"""
                            , typeDef = """{ type : "OnlyVariant";  }"""
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
                            , output = """{"type":"Alert","args":["Hello!"]}"""
                            , typeDef = """{ type : "Alert"; args: [ string ]; } | { type : "SendPresenceHeartbeat";  }"""
                            }
            , test "merge object to variant" <|
                \() ->
                    let
                        alertObjectEncoder =
                            TsPort.build
                                |> property "message" TsPort.string
                    in
                    TsPort.custom
                        (\vSendHeartbeat vAlert value ->
                            case value of
                                SendPresenceHeartbeat ->
                                    vSendHeartbeat

                                Alert string ->
                                    vAlert string
                        )
                        |> TsPort.variant0 "SendPresenceHeartbeat"
                        |> TsPort.objectVariant "Alert" alertObjectEncoder
                        |> TsPort.buildCustom
                        |> expectEncodes
                            { input = Alert "Hello!"
                            , output = """{"type":"Alert","message":"Hello!"}"""
                            , typeDef = """{ type : "Alert"; message : string } | { type : "SendPresenceHeartbeat";  }"""
                            }
            ]
        ]


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
