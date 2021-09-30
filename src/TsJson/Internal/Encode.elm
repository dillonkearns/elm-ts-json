module TsJson.Internal.Encode exposing (Encoder(..), intersectTypes)

import Internal.TsJsonType exposing (TsType)
import Internal.TypeReducer as TypeReducer
import Json.Encode as Encode


type Encoder input
    = Encoder (input -> Encode.Value) TsType


intersectTypes : Encoder input -> TsType -> Encoder input
intersectTypes (Encoder encoder_ tsType1) tsType2 =
    Encoder encoder_ (TypeReducer.intersect tsType1 tsType2)
