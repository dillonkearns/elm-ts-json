module ScrollIntoView exposing (Alignment(..), Behavior(..), alignmentEncoder, behaviorEncoder, encoder)

import Json.Encode
import TsJson.Encode exposing (Encoder, buildUnion, object, optional, union, variantLiteral)


type Behavior
    = Auto
    | Smooth


type Alignment
    = Start
    | Center
    | End
    | Nearest


encoder :
    Encoder
        { behavior : Maybe Behavior
        , block : Maybe Alignment
        , inline : Maybe Alignment
        }
encoder =
    object
        [ optional "behavior" .behavior behaviorEncoder
        , optional "block" .block alignmentEncoder
        , optional "inline" .inline alignmentEncoder
        ]


behaviorEncoder : Encoder Behavior
behaviorEncoder =
    union
        (\vAuto vSmooth value ->
            case value of
                Auto ->
                    vAuto

                Smooth ->
                    vSmooth
        )
        |> variantLiteral (Json.Encode.string "auto")
        |> variantLiteral (Json.Encode.string "smooth")
        |> buildUnion


alignmentEncoder : Encoder Alignment
alignmentEncoder =
    union
        (\vStart vCenter vEnd vNearest value ->
            case value of
                Start ->
                    vStart

                Center ->
                    vCenter

                End ->
                    vEnd

                Nearest ->
                    vNearest
        )
        |> variantLiteral (Json.Encode.string "start")
        |> variantLiteral (Json.Encode.string "center")
        |> variantLiteral (Json.Encode.string "end")
        |> variantLiteral (Json.Encode.string "nearest")
        |> buildUnion
