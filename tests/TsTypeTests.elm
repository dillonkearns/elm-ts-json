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
                    TsType.combine (TsType.TypeObject []) (TsType.TypeObject [])
                        |> Expect.equal
                            (TsType.TypeObject [])
            ]
        ]
