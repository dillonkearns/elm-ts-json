module TsInterop.Decode exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


map : (value -> mapped) -> InteropDecoder value -> InteropDecoder mapped
map mapFn (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.map mapFn innerDecoder) innerType


nullable : InteropDecoder value -> InteropDecoder (Maybe value)
nullable (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.nullable innerDecoder) (Union [ innerType, Null ])


oneOf : List (InteropDecoder value) -> InteropDecoder value
oneOf decoders =
    InteropDecoder
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerDecoder
                )
            |> Decode.oneOf
        )
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerType
                )
            |> Union
        )


type TsType
    = String
    | List TsType
    | Literal Encode.Value
    | Union (List TsType)
    | Null


type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


literal : value -> Encode.Value -> InteropDecoder value
literal value literalValue =
    InteropDecoder
        (Decode.value
            |> Decode.andThen
                (\decodeValue ->
                    if literalValue == decodeValue then
                        Decode.succeed value

                    else
                        Decode.fail ("Expected the following literal value: " ++ Encode.encode 0 literalValue)
                )
        )
        (Literal literalValue)


string : InteropDecoder String
string =
    InteropDecoder Decode.string String


list : InteropDecoder value -> InteropDecoder (List value)
list (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.list innerDecoder) (List innerType)


decoder : InteropDecoder value -> Decoder value
decoder (InteropDecoder decoder_ tsType_) =
    decoder_


tsTypeToString : InteropDecoder value -> String
tsTypeToString (InteropDecoder decoder_ tsType_) =
    tsTypeToString_ tsType_


tsTypeToString_ : TsType -> String
tsTypeToString_ tsType_ =
    case tsType_ of
        String ->
            "string"

        List listType ->
            tsTypeToString_ listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue

        Union tsTypes ->
            tsTypes
                |> List.map tsTypeToString_
                |> String.join " | "

        Null ->
            "null"
