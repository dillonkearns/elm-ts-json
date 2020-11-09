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
            , test "custom type" <|
                \() ->
                    list string
                        |> expectEncodes
                            { input = [ "Item 1", "Item 2" ]
                            , output = "[\"Item 1\",\"Item 2\"]"
                            , typeDef = "string[]"
                            }
            ]
        ]



--topLevelEncoder : (encodeValue1 -> encodesFrom) -> Interop Never encodesFrom
--initToJs : value -> Interop Never value


initToJs =
    Debug.todo ""


builder : String -> (value -> toJsPayload) -> Interop Never (value -> encodesFrom) -> Interop Never encodesFrom
builder variant function interop =
    Debug.todo ""


type ToJs
    = Popup String
    | Record { a : String, b : String }


succeed : decodesTo -> Interop decodesTo Never
succeed value =
    Interop (Json.Decode.succeed value) (\_ -> Encode.null) ""



--topLevelEncoderExample : Interop Never ToJs
--topLevelEncoderExample =
--    --succeed ()
--    initToJs
--        |> builder "Popup" string


expectDecodes :
    { input : String, output : decodesTo, typeDef : String }
    -> Interop decodesTo encodesFrom
    -> Expect.Expectation
expectDecodes expect interop =
    expect.input
        |> Json.Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


expectEncodes :
    { output : String, input : encodesFrom, typeDef : String }
    -> Interop decodesTo encodesFrom
    -> Expect.Expectation
expectEncodes expect interop =
    expect.input
        |> encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


encoder : Interop decodesTo encodesFrom -> (encodesFrom -> Encode.Value)
encoder interop =
    \_ -> Encode.list Encode.string [ "Item 1", "Item 2" ]


typeDef (Interop jsonDecoder encoder_ typeDef_) =
    typeDef_


string : Interop String String
string =
    Interop Json.Decode.string Encode.string "string"


list : Interop decodesTo encodesFrom -> Interop (List decodesTo) (List encodesFrom)
list (Interop decoder1 encoder1 annotation1) =
    Interop (Json.Decode.list decoder1) (Encode.list encoder1) (annotation1 ++ "[]")


type Interop decodesTo encodesFrom
    = Interop (Json.Decode.Decoder decodesTo) (encodesFrom -> Encode.Value) String


decoder : Interop decodesTo encodesFrom -> Json.Decode.Decoder decodesTo
decoder (Interop jsonDecoder encoder_ typeDef_) =
    jsonDecoder



--example : Interop Never ToJs
--example =
--    Interop (Json.Decode.fail "")
--        (\toJs ->
--            case toJs of
--                Popup string_ ->
--                    ( "Popup", string_ "" )
--
--                Record record ->
--                    ( "Record", string_ "" )
--        )
--        ""
