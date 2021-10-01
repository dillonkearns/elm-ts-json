module TsJson.Internal.Codec exposing (Codec(..))

import TsJson.Decode as JD
import TsJson.Encode exposing (Encoder, Property)


type Codec a
    = Codec
        { encoder : Encoder a
        , decoder : JD.Decoder a
        }
