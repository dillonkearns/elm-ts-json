module TsTypeTests exposing (suite)

import Dict exposing (Dict)
import Expect
import Json.Decode
import Json.Diff
import Json.Encode
import Json.Patch
import Json.Value
import JsonDiff
import Test exposing (..)
import TsType exposing (PropertyOptionality(..), TsType(..))


suite : Test
suite =
    describe "TsType"
        [ describe "combine"
            [ test "two object types are merged" <|
                \() ->
                    TsType.intersect
                        (TsType.TypeObject [])
                        (TsType.TypeObject [])
                        |> Expect.equal
                            (TsType.TypeObject [])
            , test "arrays with complementary indices are merged" <|
                \() ->
                    TsType.intersect
                        (TsType.ArrayIndex ( 0, TsType.String ) [])
                        (TsType.ArrayIndex ( 1, TsType.Number ) [])
                        |> expectEqualTypes
                            "[string,number,...JsonValue[]]"
            , test "a known value intersected with JsonValue is the known value" <|
                \() ->
                    TsType.intersect
                        TsType.Boolean
                        TsType.Unknown
                        |> Expect.equal TsType.Boolean
            , test "merge object type into union of objects" <|
                \() ->
                    TsType.intersect
                        (TsType.TypeObject [ ( Required, "version", Number ) ])
                        (TsType.union
                            [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                            , TypeObject [ ( Required, "payload", String ) ]
                            ]
                        )
                        |> Expect.equal
                            (TsType.Intersection
                                [ TsType.TypeObject [ ( Required, "version", Number ) ]
                                , TsType.union
                                    [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                    , TypeObject [ ( Required, "payload", String ) ]
                                    ]
                                ]
                            )
            , test "object fields are merged together" <|
                \() ->
                    TsType.intersect
                        (TsType.TypeObject [ ( Required, "version", Number ) ])
                        (TsType.TypeObject [ ( Required, "author", TsType.String ) ])
                        |> Expect.equal
                            (TsType.TypeObject
                                [ ( Required, "version", Number )
                                , ( Required, "author", TsType.String )
                                ]
                            )
            , test "all objects in intersection are merged" <|
                \() ->
                    TsType.TypeObject [ ( Required, "author", String ) ]
                        |> TsType.intersect
                            (TsType.TypeObject [ ( Required, "version", Number ) ])
                        |> TsType.intersect
                            (TsType.TypeObject [ ( Required, "license", String ) ])
                        |> Expect.equal
                            (TsType.TypeObject
                                [ ( Required, "license", String )
                                , ( Required, "version", Number )
                                , ( Required, "author", String )
                                ]
                            )
            , test "intersections are merged" <|
                \() ->
                    TsType.intersect
                        (TsType.Intersection
                            [ TsType.TypeObject [ ( Required, "version", Number ) ]
                            , TsType.union
                                [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                , TypeObject [ ( Required, "payload", String ) ]
                                ]
                            ]
                        )
                        (TsType.Intersection
                            [ TsType.TypeObject [ ( Required, "author", String ) ]
                            , TsType.union
                                [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                , TypeObject [ ( Required, "payload", String ) ]
                                ]
                            ]
                        )
                        |> expectEqualTypes
                            "({ version : number; author : string } & { data : { payload : string } } | { payload : string })"
            , test "contradictory scalars" <|
                \() ->
                    combinesToNever TsType.String TsType.Number
            , test "contradictory scalars reversed" <|
                \() ->
                    combinesToNever TsType.Number TsType.String
            , test "never in a union is factored out" <|
                \() ->
                    TsType.union [ TsType.TsNever, TsType.String ]
                        |> Expect.equal TsType.String
            , test "unioning with just a single never is never" <|
                \() ->
                    TsType.union [ TsType.TsNever ]
                        |> Expect.equal TsType.TsNever
            ]
        , describe "parenthesized when needed"
            [ test "list of union" <|
                \() ->
                    TsType.List
                        (TsType.union
                            [ TsType.String
                            , TsType.Number
                            ]
                        )
                        |> TsType.toString
                        |> Expect.equal "(string | number)[]"
            ]
        , describe "toJsonSchema"
            [ test "string" <|
                \() ->
                    TsType.String
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"string"}"""
            , test "number" <|
                \() ->
                    TsType.Number
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"number"}"""
            , test "integer" <|
                \() ->
                    TsType.Integer
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"integer"}"""
            , test "boolean" <|
                \() ->
                    TsType.Boolean
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"boolean"}"""
            , test "array" <|
                \() ->
                    TsType.List TsType.String
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"array","items":{"type":"string"}}"""
            , test "unknown" <|
                \() ->
                    TsType.Unknown
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{}"""
            , test "literal string" <|
                \() ->
                    TsType.Literal (Json.Encode.string "literalString")
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"const":"literalString"}"""
            , test "union" <|
                \() ->
                    TsType.Union
                        ( TsType.Literal (Json.Encode.string "guest")
                        , [ TsType.Literal (Json.Encode.string "admin") ]
                        )
                        |> TsType.toJsonSchema
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
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"allOf":[{"type":"object","properties":{"first":{"type":"string"}},"required":["first"]},{"type":"object","properties":{"last":{"type":"string"}},"required":["last"]}]}"""
            , test "tuple" <|
                \() ->
                    TsType.Tuple
                        [ TsType.String, TsType.Boolean ]
                        Nothing
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"}],"maxItems":2,"minItems":2,"type":"array"}"""
            , test "3-tuple" <|
                \() ->
                    TsType.Tuple
                        [ TsType.String, TsType.Boolean, TsType.Number ]
                        Nothing
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"items":[{"type":"string"},{"type":"boolean"},{"type":"number"}],"maxItems":3,"minItems":3,"type":"array"}"""
            , test "tuple with rest elements" <|
                \() ->
                    TsType.Tuple
                        [ TsType.String, TsType.Boolean ]
                        (Just TsType.Number)
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"additionalItems":{"type":"number"},"items":[{"type":"string"},{"type":"boolean"}],"minItems":2,"type":"array"}"""
            , test "object with uniform values" <|
                \() ->
                    TsType.ObjectWithUniformValues TsType.Boolean
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"additionalProperties":{"type":"boolean"},"type":"object"}"""
            , test "object with no required properties" <|
                \() ->
                    TsType.TypeObject
                        [ ( TsType.Optional, "first", TsType.String )
                        , ( TsType.Optional, "middle", TsType.String )
                        , ( TsType.Optional, "last", TsType.String )
                        ]
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}}}"""
            , test "object with some required properties" <|
                \() ->
                    TsType.TypeObject
                        [ ( TsType.Required, "first", TsType.String )
                        , ( TsType.Optional, "middle", TsType.String )
                        , ( TsType.Required, "last", TsType.String )
                        ]
                        |> TsType.toJsonSchema
                        |> Json.Encode.encode 0
                        |> Expect.equal """{"type":"object","properties":{"first":{"type":"string"},"middle":{"type":"string"},"last":{"type":"string"}},"required":["first","last"]}"""
            ]
        ]


expectEqualTypes : String -> TsType -> Expect.Expectation
expectEqualTypes expected type2 =
    TsType.toString type2
        |> Expect.equal expected


combinesToNever : TsType -> TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.intersect type1 type2
        |> Expect.equal TsType.TsNever


equalJson2 jsonString1 jsonString2 =
    let
        json1 =
            jsonString1
                |> Json.Decode.decodeString Json.Value.decoder

        json2 =
            jsonString2
                |> Json.Decode.decodeString Json.Value.decoder
    in
    case ( json1, json2 ) of
        ( Ok ok1, Ok ok2 ) ->
            JsonDiff.expectEqual (JsonDiff.toUnorderedJsonValue ok1) (JsonDiff.toUnorderedJsonValue ok2)

        _ ->
            Expect.fail "Invalid JSON."


equalJson : String -> String -> Expect.Expectation
equalJson jsonString2 jsonString1 =
    let
        json1 =
            jsonString1
                |> Json.Decode.decodeString Json.Decode.value

        json2 =
            jsonString2
                |> Json.Decode.decodeString Json.Decode.value
    in
    case ( json1, json2 ) of
        ( Ok ok1, Ok ok2 ) ->
            Expect.equalLists
                (Json.Encode.encode 2 ok1 |> String.lines)
                (Json.Encode.encode 2 ok2 |> String.lines)

        _ ->
            Expect.fail "Invalid JSON."


patchToString : Json.Encode.Value -> Json.Encode.Value -> Json.Patch.Operation -> String
patchToString json1 json2 patch =
    case patch of
        Json.Patch.Replace pointer jsonValue ->
            (pointer |> String.join ".")
                ++ Json.Encode.encode 2 jsonValue
                ++ "\n\nJSON 1:\n"
                ++ Json.Encode.encode 2 json1
                ++ "\n\nJSON 2:\n"
                ++ Json.Encode.encode 2 json2

        Json.Patch.Add pointer jsonValue ->
            (pointer |> String.join ".")
                ++ Json.Encode.encode 2 jsonValue
                ++ "\n\nJSON 1:\n"
                ++ Json.Encode.encode 2 json1
                ++ "\n\nJSON 2:\n"
                ++ Json.Encode.encode 2 json2

        Json.Patch.Remove pointer ->
            "Missing key `" ++ String.join "." pointer ++ "`"

        _ ->
            "TODO"
