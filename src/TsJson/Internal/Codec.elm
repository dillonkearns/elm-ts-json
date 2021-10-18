module TsJson.Internal.Codec exposing (Codec(..))

import TsJson.Decode as TsDecode
import TsJson.Encode exposing (Encoder, Property)


type Codec a
    = Codec
        { encoder : Encoder a
        , decoder : TsDecode.Decoder a
        }
