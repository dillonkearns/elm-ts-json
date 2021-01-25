module TsJson.Type exposing (..)

{-| -}

import Internal.TsJsonType
import Json.Encode
import TsType


{-| -}
type alias Type =
    Internal.TsJsonType.TsType


{-| -}
toJsonSchema : Type -> Json.Encode.Value
toJsonSchema =
    TsType.toJsonSchema


{-| -}
toString : Type -> String
toString =
    TsType.toString
