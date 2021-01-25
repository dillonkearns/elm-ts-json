module TsJson.Type exposing
    ( Type
    , toTypeScript, toJsonSchema
    )

{-|

@docs Type
@docs toTypeScript, toJsonSchema

-}

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
toTypeScript : Type -> String
toTypeScript =
    TsType.toString
