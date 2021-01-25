module TsTypeTests exposing (suite)

import Expect
import Internal.TsJsonType exposing (..)
import Internal.TypeReducer as TypeReducer
import Internal.TypeToString as TypeToString
import Json.Decode
import Json.Encode
import Test exposing (..)


suite : Test
suite =
    describe "TsType"
        [ describe "combine"
            [ test "two object types are merged" <|
                \() ->
                    TypeReducer.intersect
                        (TypeObject [])
                        (TypeObject [])
                        |> Expect.equal
                            (TypeObject [])
            , test "arrays with complementary indices are merged" <|
                \() ->
                    TypeReducer.intersect
                        (ArrayIndex ( 0, String ) [])
                        (ArrayIndex ( 1, Number ) [])
                        |> expectEqualTypes
                            "[string,number,...JsonValue[]]"
            , test "a known value intersected with JsonValue is the known value" <|
                \() ->
                    TypeReducer.intersect
                        Boolean
                        Unknown
                        |> Expect.equal Boolean
            , test "merge object type into union of objects" <|
                \() ->
                    TypeReducer.intersect
                        (TypeObject [ ( Required, "version", Number ) ])
                        (TypeReducer.union
                            [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                            , TypeObject [ ( Required, "payload", String ) ]
                            ]
                        )
                        |> Expect.equal
                            (Intersection
                                [ TypeObject [ ( Required, "version", Number ) ]
                                , TypeReducer.union
                                    [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                    , TypeObject [ ( Required, "payload", String ) ]
                                    ]
                                ]
                            )
            , test "object fields are merged together" <|
                \() ->
                    TypeReducer.intersect
                        (TypeObject [ ( Required, "version", Number ) ])
                        (TypeObject [ ( Required, "author", String ) ])
                        |> Expect.equal
                            (TypeObject
                                [ ( Required, "version", Number )
                                , ( Required, "author", String )
                                ]
                            )
            , test "all objects in intersection are merged" <|
                \() ->
                    TypeObject [ ( Required, "author", String ) ]
                        |> TypeReducer.intersect
                            (TypeObject [ ( Required, "version", Number ) ])
                        |> TypeReducer.intersect
                            (TypeObject [ ( Required, "license", String ) ])
                        |> Expect.equal
                            (TypeObject
                                [ ( Required, "license", String )
                                , ( Required, "version", Number )
                                , ( Required, "author", String )
                                ]
                            )
            , test "intersections are merged" <|
                \() ->
                    TypeReducer.intersect
                        (Intersection
                            [ TypeObject [ ( Required, "version", Number ) ]
                            , TypeReducer.union
                                [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                , TypeObject [ ( Required, "payload", String ) ]
                                ]
                            ]
                        )
                        (Intersection
                            [ TypeObject [ ( Required, "author", String ) ]
                            , TypeReducer.union
                                [ TypeObject [ ( Required, "data", TypeObject [ ( Required, "payload", String ) ] ) ]
                                , TypeObject [ ( Required, "payload", String ) ]
                                ]
                            ]
                        )
                        |> expectEqualTypes
                            "({ version : number; author : string } & { data : { payload : string } } | { payload : string })"
            , test "contradictory scalars" <|
                \() ->
                    combinesToNever String Number
            , test "contradictory scalars reversed" <|
                \() ->
                    combinesToNever Number String
            , test "never in a union is factored out" <|
                \() ->
                    TypeReducer.union [ TsNever, String ]
                        |> Expect.equal String
            , test "unioning with just a single never is never" <|
                \() ->
                    TypeReducer.union [ TsNever ]
                        |> Expect.equal TsNever
            ]
        , describe "parenthesized when needed"
            [ test "list of union" <|
                \() ->
                    List
                        (TypeReducer.union
                            [ String
                            , Number
                            ]
                        )
                        |> TypeToString.toString
                        |> Expect.equal "(string | number)[]"
            ]
        ]


expectEqualTypes : String -> TsType -> Expect.Expectation
expectEqualTypes expected type2 =
    TypeToString.toString type2
        |> Expect.equal expected


combinesToNever : TsType -> TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TypeReducer.intersect type1 type2
        |> Expect.equal TsNever
