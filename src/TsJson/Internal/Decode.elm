module TsJson.Internal.Decode exposing (Decoder(..))

import Internal.TsJsonType exposing (TsType)
import Json.Decode as Decode


type Decoder value
    = Decoder (Decode.Decoder value) TsType
