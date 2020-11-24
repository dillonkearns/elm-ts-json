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
                    TsType.combine
                        (TsType.TypeObject [])
                        (TsType.TypeObject [])
                        |> Expect.equal
                            (TsType.TypeObject [])
            , test "merge object type into union of objects" <|
                \() ->
                    TsType.combine
                        (TsType.TypeObject [ ( "version", Number ) ])
                        (TsType.Union
                            [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                            , TypeObject [ ( "payload", String ) ]
                            ]
                        )
                        |> Expect.equal
                            (TsType.Intersection
                                [ TsType.TypeObject [ ( "version", Number ) ]
                                , TsType.Union
                                    [ TypeObject [ ( "data", TypeObject [ ( "payload", String ) ] ) ]
                                    , TypeObject [ ( "payload", String ) ]
                                    ]
                                ]
                            )
            , test "contradictory scalars" <|
                \() ->
                    combinesToNever TsType.String TsType.Number
            , test "contradictory scalars reversed" <|
                \() ->
                    combinesToNever TsType.Number TsType.String
            ]
        , describe "parenthesized when needed"
            [ test "list of union" <|
                \() ->
                    TsType.List
                        (TsType.Union
                            [ TsType.String
                            , TsType.Number
                            ]
                        )
                        |> TsType.tsTypeToString_
                        |> Expect.equal "(string | number)[]"
            ]
        ]


combinesToNever : TsType.TsType -> TsType.TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.combine type1 type2
        |> Expect.equal TsType.TsNever
