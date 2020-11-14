module TsInteropDecodeTests exposing (..)

import Expect exposing (Expectation)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Interop decode"
        [ test "standalone string" <|
            \() ->
                string
                    |> expectDecodes
                        { input = "\"Dillon\""
                        , output = "Dillon"
                        , typeDef = "string"
                        }
        , test "list of strings" <|
            \() ->
                list string
                    |> expectDecodes
                        { input = """["Hello", "World"]"""
                        , output = [ "Hello", "World" ]
                        , typeDef = "string[]"
                        }
        , test "literal" <|
            \() ->
                literal "Hello" (Encode.list Encode.string [ "Hello" ])
                    |> expectDecodes
                        { input = "[\"Hello\"]"
                        , output = "Hello"
                        , typeDef = "[\"Hello\"]"
                        }
        , test "literals" <|
            \() ->
                oneOf
                    [ literal Info (Encode.string "info")
                    , literal Warning (Encode.string "warning")
                    , literal Error (Encode.string "error")
                    ]
                    |> expectDecodes
                        { input = "\"info\""
                        , output = Info
                        , typeDef = "\"info\" | \"warning\" | \"error\""
                        }
        , test "nullable" <|
            \() ->
                nullable string
                    |> expectDecodes
                        { input = "null"
                        , output = Nothing
                        , typeDef = "string | null"
                        }
        , test "map" <|
            \() ->
                string
                    |> map String.toInt
                    |> expectDecodes
                        { input = "\"123\""
                        , output = Just 123
                        , typeDef = "string"
                        }
        ]


map : (value -> mapped) -> InteropDecoder value -> InteropDecoder mapped
map mapFn (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.map mapFn innerDecoder) innerType


nullable : InteropDecoder value -> InteropDecoder (Maybe value)
nullable (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.nullable innerDecoder) (Union [ innerType, Null ])


oneOf : List (InteropDecoder value) -> InteropDecoder value
oneOf decoders =
    InteropDecoder
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerDecoder
                )
            |> Decode.oneOf
        )
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerType
                )
            |> Union
        )


type Severity
    = Info
    | Warning
    | Error


type TsType
    = String
    | List TsType
    | Literal Encode.Value
    | Union (List TsType)
    | Null


type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


literal : value -> Encode.Value -> InteropDecoder value
literal value literalValue =
    InteropDecoder
        (Decode.value
            |> Decode.andThen
                (\decodeValue ->
                    if literalValue == decodeValue then
                        Decode.succeed value

                    else
                        Decode.fail ("Expected the following literal value: " ++ Encode.encode 0 literalValue)
                )
        )
        (Literal literalValue)


string : InteropDecoder String
string =
    InteropDecoder Decode.string String


list : InteropDecoder value -> InteropDecoder (List value)
list (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.list innerDecoder) (List innerType)


decoder : InteropDecoder value -> Decoder value
decoder (InteropDecoder decoder_ tsType_) =
    decoder_


tsTypeToString : InteropDecoder value -> String
tsTypeToString (InteropDecoder decoder_ tsType_) =
    tsTypeToString_ tsType_


tsTypeToString_ : TsType -> String
tsTypeToString_ tsType_ =
    case tsType_ of
        String ->
            "string"

        List listType ->
            tsTypeToString_ listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue

        Union tsTypes ->
            tsTypes
                |> List.map tsTypeToString_
                |> String.join " | "

        Null ->
            "null"


expectDecodes :
    { output : decodesTo, input : String, typeDef : String }
    -> InteropDecoder decodesTo
    -> Expect.Expectation
expectDecodes expect interop =
    expect.input
        |> Decode.decodeString (decoder interop)
        |> Expect.all
            [ \decoded -> decoded |> Expect.equal (Ok expect.output)
            , \decoded -> tsTypeToString interop |> Expect.equal expect.typeDef
            ]
