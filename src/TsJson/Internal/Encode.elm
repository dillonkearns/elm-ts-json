module TsJson.Internal.Encode exposing (Encoder(..))

import Internal.TsJsonType exposing (TsType)
import Json.Encode as Encode


type Encoder input
    = Encoder (input -> Encode.Value) TsType
