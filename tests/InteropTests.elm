module InteropTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import Test exposing (..)


suite : Test
suite =
    describe "Interop"
        [ test "string" <|
            \() ->
                string
                    |> expectInterop
                        { input = "\"Hello!\""
                        , output = "Hello!"
                        , typeDef = "string"
                        }
        ]


expectInterop :
    { input : String, output : value, typeDef : String }
    -> Interop value
    -> Expect.Expectation
expectInterop expect interop =
    expect.input
        |> Json.Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


typeDef (Interop jsonDecoder typeDef_) =
    typeDef_


string : Interop String
string =
    Interop Json.Decode.string "string"


type Interop decodesTo
    = Interop (Json.Decode.Decoder decodesTo) String


decoder : Interop value -> Json.Decode.Decoder value
decoder (Interop jsonDecoder typeDef_) =
    jsonDecoder
