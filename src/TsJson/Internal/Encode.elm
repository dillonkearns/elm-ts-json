module TsJson.Internal.Encode exposing (Encoder(..), UnionBuilder(..), UnionEncodeValue(..))

import Internal.TsJsonType exposing (TsType)
import Json.Encode as Encode


type Encoder input
    = Encoder (input -> Encode.Value) TsType


{-| -}
type UnionBuilder match
    = UnionBuilder match (List TsType)


type UnionEncodeValue
    = UnionEncodeValue Encode.Value
