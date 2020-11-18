module GoalPortsFromTs exposing (..)

import Json.Encode as Encode
import TsInterop.Decode as Decode exposing (InteropDecoder, literal, oneOf)


type Severity
    = Info
    | Warning
    | Error


flags : InteropDecoder Severity
flags =
    oneOf
        [ literal Info (Encode.string "info")
        , literal Warning (Encode.string "warning")
        , literal Error (Encode.string "error")
        ]
