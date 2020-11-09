module InteropTests2 exposing (..)

import Expect exposing (Expectation)
import Json.Decode
import Json.Encode as Encode
import Test exposing (..)


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
            [ test "list string" <|
                \() ->
                    build
                        |> string "first" .first
                        |> string "last" .last
                        |> toEncoder
                        |> expectEncodes
                            { input = { first = "Dillon", last = "Kearns" }
                            , output = """{"last":"Kearns","first":"Dillon"}"""
                            , typeDef = "{ first : string; last : string }"
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
        |> encoder interop
        |> Encode.encode 0
        |> Expect.all
            [ \encodedString -> encodedString |> Expect.equal expect.output
            , \decoded -> expect.typeDef |> Expect.equal (typeDef interop)
            ]


type Encoder encodesFrom
    = Encoder (encodesFrom -> EncodeValue)


encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn) encodesFrom =
    encodeFn encodesFrom
        |> serializeEncodeValue


serializeEncodeValue : EncodeValue -> Encode.Value
serializeEncodeValue value =
    case value of
        --(List ( String, Encode.Value, TsType ))
        Object list_ ->
            list_
                |> Debug.log "value"
                |> List.map
                    (\( objectKey, objectValue, tsType ) ->
                        ( objectKey, objectValue )
                    )
                |> Encode.object


typeDef : Encoder encodesFrom -> String
typeDef encoder_ =
    "{ first : string; last : string }"


type EncodeValue
    = Object (List ( String, Encode.Value, TsType ))


type ObjectBuilder encodesFrom
    = ObjectBuilder (encodesFrom -> List ( String, Encode.Value, TsType ))


type TsType
    = String
    | List TsType


build : ObjectBuilder encodesFrom
build =
    ObjectBuilder (\_ -> [])


string : String -> (value -> String) -> ObjectBuilder value -> ObjectBuilder value
string keyName getter (ObjectBuilder builderFn) =
    ObjectBuilder
        (\encodesFrom ->
            ( keyName, Encode.string (getter encodesFrom), String )
                :: builderFn encodesFrom
        )


list : String -> (value -> List String) -> ObjectBuilder value -> ObjectBuilder value
list keyName getter (ObjectBuilder builderFn) =
    ObjectBuilder
        (\encodesFrom ->
            ( keyName, Encode.list Encode.string (getter encodesFrom), List String )
                :: builderFn encodesFrom
        )


personEncoder : Encoder { first : String, last : String }
personEncoder =
    build
        |> string "first" .first
        |> string "last" .last
        |> toEncoder


toEncoder : ObjectBuilder value -> Encoder value
toEncoder (ObjectBuilder builderFn) =
    Encoder (\encodesFrom -> Object (builderFn encodesFrom))


tsAnnotation : Encoder value -> value -> String
tsAnnotation (Encoder encodeFn) value =
    case encodeFn value of
        Object list_ ->
            list_
                |> List.map
                    (\( fieldName, encodeValue, tsType ) ->
                        fieldName ++ " : " ++ tsTypeToString tsType
                    )
                |> String.join " ; "


tsTypeToString : TsType -> String
tsTypeToString tsType =
    case tsType of
        String ->
            ""

        List listType ->
            ""
