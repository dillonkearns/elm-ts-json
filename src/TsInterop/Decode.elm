module TsInterop.Decode exposing
    ( InteropDecoder
    , succeed
    , bool, float, int, string
    , list, nullable, oneOf
    , map, map2
    , literal
    , decoder, tsTypeToString
    , field, map3
    )

{-|


## Decoders

@docs InteropDecoder

@docs succeed


## Built-Ins

@docs bool, float, int, string


## Composite Types

@docs list, nullable, oneOf


## Transformations

@docs map, map2


## TypeScript Literals

@docs literal


## Using Decoders

@docs decoder, tsTypeToString

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import TsType exposing (TsType(..))


{-| -}
map : (value -> mapped) -> InteropDecoder value -> InteropDecoder mapped
map mapFn (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.map mapFn innerDecoder) innerType


{-| -}
map2 : (value1 -> value2 -> mapped) -> InteropDecoder value1 -> InteropDecoder value2 -> InteropDecoder mapped
map2 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) =
    InteropDecoder (Decode.map2 mapFn innerDecoder1 innerDecoder2) (TsType.combine innerType1 innerType2)


{-| -}
map3 : (value1 -> value2 -> value3 -> mapped) -> InteropDecoder value1 -> InteropDecoder value2 -> InteropDecoder value3 -> InteropDecoder mapped
map3 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) =
    InteropDecoder (Decode.map3 mapFn innerDecoder1 innerDecoder2 innerDecoder3)
        (TsType.combine
            innerType1
            innerType2
            |> TsType.combine innerType3
        )


{-| -}
nullable : InteropDecoder value -> InteropDecoder (Maybe value)
nullable (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.nullable innerDecoder) (Union [ innerType, TsType.null ])


{-| -}
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


{-| -}
type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


{-| -}
succeed : value -> InteropDecoder value
succeed value =
    InteropDecoder (Decode.succeed value) TsType.Unknown


{-| TypeScript has support for literals.
-}
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


{-| -}
field : String -> InteropDecoder value -> InteropDecoder value
field fieldName (InteropDecoder innerDecoder innerType) =
    InteropDecoder
        (Decode.field fieldName innerDecoder)
        (TsType.TypeObject [ ( fieldName, innerType ) ])


{-| -}
string : InteropDecoder String
string =
    InteropDecoder Decode.string String


{-| -}
int : InteropDecoder Int
int =
    InteropDecoder Decode.int TsType.Number


{-| -}
float : InteropDecoder Float
float =
    InteropDecoder Decode.float TsType.Number


{-| -}
bool : InteropDecoder Bool
bool =
    InteropDecoder Decode.bool TsType.Boolean


{-| -}
list : InteropDecoder value -> InteropDecoder (List value)
list (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.list innerDecoder) (List innerType)


{-| -}
decoder : InteropDecoder value -> Decoder value
decoder (InteropDecoder decoder_ tsType_) =
    decoder_


{-| -}
tsTypeToString : InteropDecoder value -> String
tsTypeToString (InteropDecoder decoder_ tsType_) =
    TsType.tsTypeToString_ tsType_
