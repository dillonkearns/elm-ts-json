module TsTypeTests exposing (suite)

import Expect
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
        ]


expectEqualTypes : String -> TsType -> Expect.Expectation
expectEqualTypes expected type2 =
    TsType.toString type2
        |> Expect.equal expected


combinesToNever : TsType -> TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.intersect type1 type2
        |> Expect.equal TsType.TsNever
