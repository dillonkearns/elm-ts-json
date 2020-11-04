module InteropTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Interop"
        [ describe "decode"
            [ test "string" <|
                \() ->
                    string
                        |> expectDecodes
                            { input = "\"Hello!\""
                            , output = "Hello!"
                            , typeDef = "string"
                            }
            , test "list string" <|
                \() ->
                    list string
                        |> expectDecodes
                            { input = "[\"Item 1\", \"Item 2\"]"
                            , output = [ "Item 1", "Item 2" ]
                            , typeDef = "string[]"
                            }
            ]
        , describe "encode"
            [ test "list string" <|
                \() ->
                    list string
                        |> expectEncodes
                            { input = [ "Item 1", "Item 2" ]
                            , output = "[\"Item 1\",\"Item 2\"]"
                            , typeDef = "string[]"
                            }
            ]
        ]


expectDecodes :
    { input : String, output : value, typeDef : String }
    -> Interop value
    -> Expect.Expectation
expectDecodes expect interop =
    expect.input
        |> Json.Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


expectEncodes :
    { output : String, input : value, typeDef : String }
    -> Interop value
    -> Expect.Expectation
expectEncodes expect interop =
    expect.input
        |> encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


encoder : Interop value -> (value -> Encode.Value)
encoder interop =
    \_ -> Encode.list Encode.string [ "Item 1", "Item 2" ]


typeDef (Interop jsonDecoder typeDef_) =
    typeDef_


string : Interop String
string =
    Interop Json.Decode.string "string"



--list : Interop decodesTo -> Interop (List decodesTo)


list (Interop decoder1 annotation1) =
    Interop (Json.Decode.list decoder1) (annotation1 ++ "[]")


type Interop decodesTo
    = Interop (Json.Decode.Decoder decodesTo) String


decoder : Interop value -> Json.Decode.Decoder value
decoder (Interop jsonDecoder typeDef_) =
    jsonDecoder
