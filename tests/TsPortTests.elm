module TsPortTests exposing (..)

import Expect exposing (Expectation)
import Json.Encode as Encode
import Test exposing (..)
import TsPort exposing (Encoder, property)


suite : Test
suite =
    describe "Interop"
        [ --describe "decode"
          --    [ test "string" <|
          --        \() ->
          --            string
          --                |> expectDecodes
          --                    { input = "\"Hello!\""
          --                    , output = "Hello!"
          --                    , typeDef = "string"
          --                    }
          --    , test "list string" <|
          --        \() ->
          --            list string
          --                |> expectDecodes
          --                    { input = "[\"Item 1\", \"Item 2\"]"
          --                    , output = [ "Item 1", "Item 2" ]
          --                    , typeDef = "string[]"
          --                    }
          --    ],
          describe "encode"
            [ test "object" <|
                \() ->
                    TsPort.build
                        |> property "first" (TsPort.string .first)
                        |> property "last" (TsPort.string .last)
                        |> TsPort.toEncoder
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = """{"last":"Kearns","first":"Dillon"}"""
                            , typeDef = "{ last : string; first : string }"
                            }
            , test "standalone string" <|
                \() ->
                    TsPort.string .first
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = "\"Dillon\""
                            , typeDef = "string"
                            }

            --, test "custom type" <|
            --    \() ->
            --        list string
            --            |> expectEncodes
            --                { input = [ "Item 1", "Item 2" ]
            --                , output = "[\"Item 1\",\"Item 2\"]"
            --                , typeDef = "string[]"
            --                }
            ]
        ]


type ToJs
    = Popup String
    | Record { a : String, b : String }


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
