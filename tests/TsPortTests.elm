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
            , test "custom type with single variant" <|
                \() ->
                    let
                        thing : TsPort.CustomBuilder (Encode.Value -> ToJs -> Encode.Value)
                        thing =
                            TsPort.custom
                                (\vSendHeartbeat value ->
                                    --case value of
                                    --    SendPresenceHeartbeat ->
                                    --        --vSendHeartbeat
                                    --        Debug.todo ""
                                    --Debug.todo ""
                                    case value of
                                        SendPresenceHeartbeat ->
                                            vSendHeartbeat
                                )
                    in
                    thing
                        |> TsPort.variant0 "SendPresenceHeartbeat"
                        |> TsPort.buildCustom
                        |> expectEncodes
                            { input = SendPresenceHeartbeat
                            , output = """{"type":"SendPresenceHeartbeat"}"""
                            , typeDef = """{ type : "SendPresenceHeartbeat" }"""
                            }
            ]
        ]


type ToJs
    = SendPresenceHeartbeat



--type ToJs
--    = Popup String
--    | Record { a : String, b : String }


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
            , \decoded -> expect.typeDef |> Expect.equal (TsPort.typeDef interop)
            ]
