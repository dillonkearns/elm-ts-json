module TsJson.Internal.Decode exposing (Decoder(..))

import Internal.TsJsonType exposing (TsType)
import Json.Decode


type Decoder value
    = Decoder (Json.Decode.Decoder value) TsType
