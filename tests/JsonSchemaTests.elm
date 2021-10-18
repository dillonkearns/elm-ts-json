module JsonSchemaTests exposing (suite)

import Expect
import Internal.JsonSchema as JsonSchema
import Internal.TsJsonType exposing (..)
import Internal.TypeReducer as TypeReducer
import Internal.TypeToString as TypeToString
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "toJsonSchema"
        [ test "string" <|
            \() ->
                String
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"string"}"""
        , test "number" <|
            \() ->
                Number
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"number"}"""
        , test "integer" <|
            \() ->
                Integer
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"integer"}"""
        , test "boolean" <|
            \() ->
                Boolean
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"boolean"}"""
        , test "array" <|
            \() ->
                List String
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"array","items":{"type":"string"}}"""
        , test "unknown" <|
            \() ->
                Unknown
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{}"""
        , test "literal string" <|
            \() ->
                Literal (Encode.string "literalString")
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"const":"literalString"}"""
        , test "union" <|
            \() ->
                Union
                    ( Literal (Encode.string "guest")
                    , [ Literal (Encode.string "admin") ]
                    )
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"anyOf":[{"const":"guest"},{"const":"admin"}]}"""
        , test "intersection" <|
            \() ->
                Intersection
                    [ TypeObject
                        [ ( Required, "first", String )
                        ]
                    , TypeObject
                        [ ( Required, "last", String )
                        ]
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"allOf":[{"type":"object","properties":{"first":{"type":"string"}},"required":["first"]},{"type":"object","properties":{"last":{"type":"string"}},"required":["last"]}]}"""
        , test "tuple" <|
            \() ->
                Tuple
                    [ String, Boolean ]
                    Nothing
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"}],"maxItems":2,"minItems":2,"type":"array"}"""
        , test "3-tuple" <|
            \() ->
                Tuple
                    [ String, Boolean, Number ]
                    Nothing
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"},{"type":"number"}],"maxItems":3,"minItems":3,"type":"array"}"""
        , test "tuple with rest elements" <|
            \() ->
                Tuple
                    [ String, Boolean ]
                    (Just Number)
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"additionalItems":{"type":"number"},"items":[{"type":"string"},{"type":"boolean"}],"minItems":2,"type":"array"}"""
        , test "object with uniform values" <|
            \() ->
                ObjectWithUniformValues Boolean
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"additionalProperties":{"type":"boolean"},"type":"object"}"""
        , test "array index" <|
            \() ->
                ArrayIndex ( 1, Boolean ) []
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"additionalItems":{},"items":[{},{"type":"boolean"}],"minItems":2,"type":"array"}"""
        , test "object with no required properties" <|
            \() ->
                TypeObject
                    [ ( Optional, "first", String )
                    , ( Optional, "middle", String )
                    , ( Optional, "last", String )
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}}}"""
        , test "object with some required properties" <|
            \() ->
                TypeObject
                    [ ( Required, "first", String )
                    , ( Optional, "middle", String )
                    , ( Required, "last", String )
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Encode.encode 0
                    |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}},"required":["first","last"]}"""
        ]


expectEqualTypes : String -> TsType -> Expect.Expectation
expectEqualTypes expected type2 =
    TypeToString.toString type2
        |> Expect.equal expected


combinesToNever : TsType -> TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TypeReducer.intersect type1 type2
        |> Expect.equal TsNever
