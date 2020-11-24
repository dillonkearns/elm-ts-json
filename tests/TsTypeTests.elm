module TsTypeTests exposing (..)

import Expect
import Test exposing (..)
import TsType exposing (TsType(..))


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
                            "[string,number,...unknown[]]"
            , test "merge object type into union of objects" <|
                \() ->
                    TsType.intersect
                        (TsType.TypeObject [ ( "version", Number ) ])
                        (TsType.union
                            [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                            , TypeObject [ ( "payload", String ) ]
                            ]
                        )
                        |> Expect.equal
                            (TsType.Intersection
                                [ TsType.TypeObject [ ( "version", Number ) ]
                                , TsType.union
                                    [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                                    , TypeObject [ ( "payload", String ) ]
                                    ]
                                ]
                            )
            , test "object fields are merged together" <|
                \() ->
                    TsType.intersect
                        (TsType.TypeObject [ ( "version", Number ) ])
                        (TsType.TypeObject [ ( "author", TsType.String ) ])
                        |> Expect.equal
                            (TsType.TypeObject
                                [ ( "version", Number )
                                , ( "author", TsType.String )
                                ]
                            )
            , test "all objects in intersection are merged" <|
                \() ->
                    TsType.TypeObject [ ( "author", String ) ]
                        |> TsType.intersect
                            (TsType.TypeObject [ ( "version", Number ) ])
                        |> TsType.intersect
                            (TsType.TypeObject [ ( "license", String ) ])
                        |> Expect.equal
                            (TsType.TypeObject
                                [ ( "license", String )
                                , ( "version", Number )
                                , ( "author", String )
                                ]
                            )
            , test "intersections are merged" <|
                \() ->
                    TsType.intersect
                        (TsType.Intersection
                            [ TsType.TypeObject [ ( "version", Number ) ]
                            , TsType.union
                                [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                                , TypeObject [ ( "payload", String ) ]
                                ]
                            ]
                        )
                        (TsType.Intersection
                            [ TsType.TypeObject [ ( "author", String ) ]
                            , TsType.union
                                [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                                , TypeObject [ ( "payload", String ) ]
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


combinesToNever : TsType.TsType -> TsType.TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.intersect type1 type2
        |> Expect.equal TsType.TsNever
