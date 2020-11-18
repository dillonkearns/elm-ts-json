module TsTypeTests exposing (..)

import Expect
import Test exposing (..)
import TsType


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
            , test "contradictory scalars" <|
                \() ->
                    combinesToNever TsType.String TsType.Number
            , test "contradictory scalars reversed" <|
                \() ->
                    combinesToNever TsType.Number TsType.String
            ]
        ]


combinesToNever : TsType.TsType -> TsType.TsType -> Expect.Expectation
combinesToNever type1 type2 =
    TsType.combine type1 type2
        |> Expect.equal TsType.TsNever
