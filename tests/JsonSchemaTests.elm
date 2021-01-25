module JsonSchemaTests exposing (suite)

import Expect
import Internal.JsonSchema as JsonSchema
import Json.Decode
import Json.Encode
import Test exposing (..)
import TsType exposing (PropertyOptionality(..), TsType(..))


suite : Test
suite =
    describe "toJsonSchema"
        [ test "string" <|
            \() ->
                TsType.String
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"string"}"""
        , test "number" <|
            \() ->
                TsType.Number
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"number"}"""
        , test "integer" <|
            \() ->
                TsType.Integer
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"integer"}"""
        , test "boolean" <|
            \() ->
                TsType.Boolean
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"boolean"}"""
        , test "array" <|
            \() ->
                TsType.List TsType.String
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"array","items":{"type":"string"}}"""
        , test "unknown" <|
            \() ->
                TsType.Unknown
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{}"""
        , test "literal string" <|
            \() ->
                TsType.Literal (Json.Encode.string "literalString")
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"const":"literalString"}"""
        , test "union" <|
            \() ->
                TsType.Union
                    ( TsType.Literal (Json.Encode.string "guest")
                    , [ TsType.Literal (Json.Encode.string "admin") ]
                    )
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"anyOf":[{"const":"guest"},{"const":"admin"}]}"""
        , test "intersection" <|
            \() ->
                TsType.Intersection
                    [ TsType.TypeObject
                        [ ( TsType.Required, "first", TsType.String )
                        ]
                    , TsType.TypeObject
                        [ ( TsType.Required, "last", TsType.String )
                        ]
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"allOf":[{"type":"object","properties":{"first":{"type":"string"}},"required":["first"]},{"type":"object","properties":{"last":{"type":"string"}},"required":["last"]}]}"""
        , test "tuple" <|
            \() ->
                TsType.Tuple
                    [ TsType.String, TsType.Boolean ]
                    Nothing
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"}],"maxItems":2,"minItems":2,"type":"array"}"""
        , test "3-tuple" <|
            \() ->
                TsType.Tuple
                    [ TsType.String, TsType.Boolean, TsType.Number ]
                    Nothing
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"},{"type":"number"}],"maxItems":3,"minItems":3,"type":"array"}"""
        , test "tuple with rest elements" <|
            \() ->
                TsType.Tuple
                    [ TsType.String, TsType.Boolean ]
                    (Just TsType.Number)
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"additionalItems":{"type":"number"},"items":[{"type":"string"},{"type":"boolean"}],"minItems":2,"type":"array"}"""
        , test "object with uniform values" <|
            \() ->
                TsType.ObjectWithUniformValues TsType.Boolean
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"additionalProperties":{"type":"boolean"},"type":"object"}"""
        , test "array index" <|
            \() ->
                TsType.ArrayIndex ( 1, TsType.Boolean ) []
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"additionalItems":{},"items":[{},{"type":"boolean"}],"minItems":2,"type":"array"}"""
        , test "object with no required properties" <|
            \() ->
                TsType.TypeObject
                    [ ( TsType.Optional, "first", TsType.String )
                    , ( TsType.Optional, "middle", TsType.String )
                    , ( TsType.Optional, "last", TsType.String )
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}}}"""
        , test "object with some required properties" <|
            \() ->
                TsType.TypeObject
                    [ ( TsType.Required, "first", TsType.String )
                    , ( TsType.Optional, "middle", TsType.String )
                    , ( TsType.Required, "last", TsType.String )
                    ]
                    |> JsonSchema.toJsonSchema
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}},"required":["first","last"]}"""
        ]


expectEqualTypes : String -> TsType -> Expect.Expectation
expectEqualTypes expected type2 =
    TsType.toString type2
        |> Expect.equal expected


combinesToNever : TsType -> TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.intersect type1 type2
        |> Expect.equal TsType.TsNever
