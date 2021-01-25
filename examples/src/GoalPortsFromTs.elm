module GoalPortsFromTs exposing (..)

import Json.Encode as Encode
import TsJson.Decode as Decode exposing (Decoder, literal, oneOf)


type Severity
    = Info
    | Warning
    | Error


type alias Person =
    { first : String
    , last : String
    , severity : Severity
    }


flags : Decoder Person
flags =
    Decode.map3 Person
        (Decode.field "first" Decode.string)
        (Decode.field "last" Decode.string)
        (Decode.field "severity" severityDecoder)


severityDecoder =
    oneOf
        [ literal Info (Encode.string "info")
        , literal Warning (Encode.string "warning")
        , literal Error (Encode.string "error")
        ]
