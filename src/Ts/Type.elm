module Ts.Type exposing (..)

{-| -}

import Json.Encode
import TsType


{-| -}
type alias TsType =
    TsType.TsType


{-| -}
toJsonSchema : TsType -> Json.Encode.Value
toJsonSchema =
    TsType.toJsonSchema


{-| -}
toString : TsType -> String
toString =
    TsType.toString
