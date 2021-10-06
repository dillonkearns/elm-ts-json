module TsJson.Codec exposing
    ( Codec
    , decoder, encoder
    , string, bool, int, float
    , maybe, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, maybeField, nullableField, buildObject
    , CustomCodec, custom, buildCustom
    , variant0
    , namedVariant1, namedVariant2
    , positionalVariant1, positionalVariant2, positionalVariant3, positionalVariant4, positionalVariant5, positionalVariant6, positionalVariant7, positionalVariant8
    , oneOf
    , map
    , succeed, recursive, fail, lazy, value, build
    , tsType
    )

{-| A `Codec a` contain a JSON `Decoder a` and the corresponding `a -> Value` encoder.

This module is a port of [`miniBill/elm-codec`](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/). Thank you for the great API design miniBill!


# Definition

@docs Codec


# Running Encoders/Decoders

@docs decoder, encoder


# Primitives

@docs string, bool, int, float


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, maybeField, nullableField, buildObject


# Custom Types

@docs CustomCodec, custom, buildCustom

@docs variant0


## Keyword Variants

@docs namedVariant1, namedVariant2


## Positional Variants

@docs positionalVariant1, positionalVariant2, positionalVariant3, positionalVariant4, positionalVariant5, positionalVariant6, positionalVariant7, positionalVariant8


# Inconsistent structure

@docs oneOf


# Mapping

@docs map


# Fancy Codecs

@docs succeed, recursive, fail, lazy, value, build

@docs tsType

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.TsJsonType as TsType exposing (TsType)
import Json.Decode exposing (Decoder)
import Json.Encode
import Set exposing (Set)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (Encoder, Property)
import TsJson.Internal.Codec exposing (Codec(..))
import TsJson.Internal.Decode
import TsJson.Internal.Encode exposing (Encoder(..), UnionBuilder(..), UnionEncodeValue(..))



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type alias Codec a =
    TsJson.Internal.Codec.Codec a



-- DECODE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> TsDecode.Decoder a
decoder (Codec m) =
    m.decoder



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> Encoder a
encoder (Codec m) =
    m.encoder



-- BASE


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : Encoder a -> TsDecode.Decoder a -> Codec a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    build TsEncode.string TsDecode.string


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build TsEncode.bool TsDecode.bool


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    build TsEncode.int TsDecode.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    build TsEncode.float TsDecode.float



-- DATA STRUCTURES


composite : (Encoder b -> Encoder a) -> (TsDecode.Decoder b -> TsDecode.Decoder a) -> Codec b -> Codec a
composite enc dec (Codec codec) =
    Codec
        { encoder = enc codec.encoder
        , decoder = dec codec.decoder
        }


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    Codec
        { decoder = TsDecode.maybe <| decoder codec
        , encoder = TsEncode.maybe <| encoder codec
        }


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list =
    composite TsEncode.list TsDecode.list


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array =
    composite TsEncode.array TsDecode.array


{-| `Codec` between a JSON object and an Elm `Dict`.
-}
dict : Codec a -> Codec (Dict String a)
dict =
    composite
        (TsEncode.dict identity)
        TsDecode.dict


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set =
    composite
        (TsEncode.map Set.toList << TsEncode.list)
        (TsDecode.map Set.fromList << TsDecode.list)


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            TsEncode.tuple
                (encoder m1)
                (encoder m2)
        , decoder =
            TsDecode.tuple
                (decoder m1)
                (decoder m2)
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            TsEncode.triple
                (encoder m1)
                (encoder m2)
                (encoder m3)
        , decoder =
            TsDecode.triple
                (decoder m1)
                (decoder m2)
                (decoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom Nothing
        (\ferr fok v ->
            case v of
                Err err ->
                    ferr err

                Ok ok ->
                    fok ok
        )
        |> positionalVariant1 "Err" Err errorCodec
        |> positionalVariant1 "Ok" Ok valueCodec
        |> buildCustom



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : List (Property a)
        , decoder : TsDecode.Decoder b
        }


{-| Start creating a `Codec` for an object. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

Example with constructor:

    type alias Point =
        { x : Float
        , y : Float
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.object Point
            |> Codec.field "x" .x Codec.float
            |> Codec.field "y" .y Codec.float
            |> Codec.buildObject

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.object (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildObject

-}
object : b -> ObjectCodec a b
object ctor =
    ObjectCodec
        { encoder = []
        , decoder = TsDecode.succeed ctor
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            TsEncode.required name getter (encoder codec)
                :: ocodec.encoder
        , decoder = TsDecode.map2 (\f x -> f x) ocodec.decoder (TsDecode.field name (decoder codec))
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`.
If the optional field's value is `Nothing` then the resulting object will not contain that field.

-}
maybeField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
maybeField name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            TsEncode.optional name getter (encoder codec)
                :: ocodec.encoder
        , decoder =
            decoder codec
                |> TsDecode.field name
                |> TsDecode.maybe
                |> TsDecode.map2 (\f x -> f x) ocodec.decoder
        }


{-| Specify the name getter and `Codec` for a required field, whose value can be `null`.

If the field is not present in the input then _the decoding fails_.
If the field's value is `Nothing` then the resulting object will contain the field with a `null` value.

This is a shorthand for a field having a codec built using `Codec.maybe`.

-}
nullableField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
nullableField name getter codec ocodec =
    field name getter (maybe codec) ocodec


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder =
            om.encoder
                |> List.reverse
                |> TsEncode.object
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : TsEncode.UnionBuilder match
        , decoder : Dict String (Json.Decode.Decoder v)
        , discriminant : Maybe String
        }


{-| Starts building a `Codec` for a custom type.

You need to pass a pattern matching function, built like this:

    import TsJson.Codec exposing (Codec)
    import TsJson.Codec.Advanced as Codec

    type Shape
        = Rectangle Int Int
        | Square Int
        | Circle Int

    shapeCodec : Codec Shape
    shapeCodec =
        Codec.custom
            (\vRectangle vSquare vCircle shape ->
                case shape of
                    Rectangle width height ->
                        vRectangle width height

                    Square width ->
                        vSquare width

                    Circle radius ->
                        vCircle radius
            )
            |> Codec.variant2 "rectangle" Rectangle Codec.int Codec.int
            |> Codec.variant1 "square" Square Codec.int
            |> Codec.variant1 "circle" Circle Codec.int
            |> Codec.buildCustomObject

    The `TsType` for `shapeCodec` is the following discriminated union:

    ```typescript
    | { shape : "circle" ; args : [ number ] }
    | { shape : "square"; args : [ number ] }
    | { shape : "rectangle"; args : [ number, number ] }
    ```

-}
custom : Maybe String -> match -> CustomCodec match value
custom discriminant match =
    CustomCodec
        { match = TsEncode.union match
        , decoder = Dict.empty
        , discriminant = discriminant
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    String
    -> decodesTo
    -> CustomCodec (TsEncode.UnionEncodeValue -> input) decodesTo
    -> CustomCodec input decodesTo
variant0 name constructor codec =
    variant_ name
        []
        (\encodeCustomTypeArgs ->
            []
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.succeed constructor
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 0 parameters for a custom type.
-}
positionalVariant1 :
    String
    -> (arg1 -> v)
    -> Codec arg1
    -> CustomCodec ((arg1 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
positionalVariant1 name constructor arg1Codec codec =
    variant_ name
        [ tsType arg1Codec
        ]
        (\encodeCustomTypeArgs a ->
            [ a |> TsEncode.encoder (encoder arg1Codec)
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map constructor
            (variantArgDecoder 0 arg1Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 2 parameters for a custom type.
-}
positionalVariant2 :
    String
    -> (arg1 -> arg2 -> v)
    -> Codec arg1
    -> Codec arg2
    -> CustomCodec ((arg1 -> arg2 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
positionalVariant2 name constructor arg1Codec arg2Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map2 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant3 :
    String
    -> (arg1 -> arg2 -> arg3 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant3 name constructor arg1Codec arg2Codec arg3Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map3 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant4 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant4 name constructor arg1Codec arg2Codec arg3Codec arg4Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            , TsEncode.encoder (encoder arg4Codec) arg4
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map4 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            (variantArgDecoder 3 arg4Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant5 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant5 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            , TsEncode.encoder (encoder arg4Codec) arg4
            , TsEncode.encoder (encoder arg5Codec) arg5
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map5 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            (variantArgDecoder 3 arg4Codec)
            (variantArgDecoder 4 arg5Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant6 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> Codec arg6
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant6 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        , tsType arg6Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 arg6 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            , TsEncode.encoder (encoder arg4Codec) arg4
            , TsEncode.encoder (encoder arg5Codec) arg5
            , TsEncode.encoder (encoder arg6Codec) arg6
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map6 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            (variantArgDecoder 3 arg4Codec)
            (variantArgDecoder 4 arg5Codec)
            (variantArgDecoder 5 arg6Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant7 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> Codec arg6
    -> Codec arg7
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant7 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec arg7Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        , tsType arg6Codec
        , tsType arg7Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 arg6 arg7 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            , TsEncode.encoder (encoder arg4Codec) arg4
            , TsEncode.encoder (encoder arg5Codec) arg5
            , TsEncode.encoder (encoder arg6Codec) arg6
            , TsEncode.encoder (encoder arg7Codec) arg7
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map7 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            (variantArgDecoder 3 arg4Codec)
            (variantArgDecoder 4 arg5Codec)
            (variantArgDecoder 5 arg6Codec)
            (variantArgDecoder 6 arg7Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
positionalVariant8 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> arg8 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> Codec arg6
    -> Codec arg7
    -> Codec arg8
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> arg8 -> TsEncode.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
positionalVariant8 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec arg7Codec arg8Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        , tsType arg6Codec
        , tsType arg7Codec
        , tsType arg8Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 ->
            [ TsEncode.encoder (encoder arg1Codec) arg1
            , TsEncode.encoder (encoder arg2Codec) arg2
            , TsEncode.encoder (encoder arg3Codec) arg3
            , TsEncode.encoder (encoder arg4Codec) arg4
            , TsEncode.encoder (encoder arg5Codec) arg5
            , TsEncode.encoder (encoder arg6Codec) arg6
            , TsEncode.encoder (encoder arg7Codec) arg7
            , TsEncode.encoder (encoder arg8Codec) arg8
            ]
                |> encodeCustomTypeArgs
                |> UnionEncodeValue
        )
        (Json.Decode.map8 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            (variantArgDecoder 2 arg3Codec)
            (variantArgDecoder 3 arg4Codec)
            (variantArgDecoder 4 arg5Codec)
            (variantArgDecoder 5 arg6Codec)
            (variantArgDecoder 6 arg7Codec)
            (variantArgDecoder 7 arg8Codec)
            |> variantArgsDecoder name
        )
        codec


variantArgDecoder : Int -> Codec a -> Json.Decode.Decoder a
variantArgDecoder index codec =
    decoder codec |> TsDecode.decoder |> Json.Decode.index index


variantArgsDecoder : String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
variantArgsDecoder expectedTagName argsDecoder =
    Json.Decode.map2 (\() v -> v)
        (Json.Decode.string
            |> Json.Decode.andThen
                (\tagName ->
                    if expectedTagName == tagName then
                        Json.Decode.succeed ()

                    else
                        Json.Decode.fail ("Expected the following tag: " ++ expectedTagName)
                )
            |> Json.Decode.field "tag"
        )
        (Json.Decode.field "args" argsDecoder)


variant_ :
    String
    -> List TsType
    -> ((List Json.Decode.Value -> Json.Decode.Value) -> a)
    -> Json.Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        discriminant =
            am.discriminant |> Maybe.withDefault "tag"

        thing =
            TsEncode.object
                [ TsEncode.required discriminant identity (TsEncode.literal (Json.Encode.string name))
                , TsEncode.required "args" identity (TsEncode.list TsEncode.value)
                ]

        enc : List Json.Encode.Value -> Json.Encode.Value
        enc =
            thing |> TsEncode.encoder

        thisType =
            TsType.TypeObject
                [ ( TsType.Required, discriminant, TsType.Literal (Json.Encode.string name) )
                , ( TsType.Required, "args", TsType.Tuple argTypes Nothing )
                ]
    in
    CustomCodec
        { match =
            case am.match of
                UnionBuilder matcher types ->
                    UnionBuilder (matcher (matchPiece enc))
                        (thisType :: types)
        , decoder = Dict.insert name decoderPiece am.decoder
        , discriminant = am.discriminant
        }


objectVariant_ :
    String
    -> List ( String, TsType )
    -> ((List ( String, TsEncode.UnionEncodeValue ) -> TsEncode.UnionEncodeValue) -> a)
    -> Json.Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
objectVariant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        discriminant =
            am.discriminant |> Maybe.withDefault "tag"

        combine : List ( String, UnionEncodeValue ) -> TsEncode.Encoder (List ( String, UnionEncodeValue ))
        combine things =
            TsJson.Internal.Encode.Encoder
                (\_ ->
                    Json.Encode.object
                        (( discriminant, Json.Encode.string name ) :: List.map (Tuple.mapSecond unwrapped) things)
                )
                thisType

        unwrapped : UnionEncodeValue -> Json.Encode.Value
        unwrapped (UnionEncodeValue rawValue) =
            rawValue

        enc : List ( String, UnionEncodeValue ) -> UnionEncodeValue
        enc props =
            props |> (combine props |> TsEncode.encoder) |> TsJson.Internal.Encode.UnionEncodeValue

        thisType : TsType
        thisType =
            TsType.TypeObject
                (( TsType.Required, discriminant, TsType.Literal (Json.Encode.string name) )
                    :: List.map (\( argName, argType ) -> ( TsType.Required, argName, argType )) argTypes
                )
    in
    CustomCodec
        { discriminant = am.discriminant
        , match =
            case am.match of
                UnionBuilder matcher types ->
                    UnionBuilder (matcher (matchPiece enc))
                        (thisType :: types)
        , decoder = Dict.insert name decoderPiece am.decoder
        }


{-| Define a variant with 1 parameter for a custom type.
-}
namedVariant1 :
    String
    -> (a -> v)
    -> ( String, Codec a )
    -> CustomCodec ((a -> UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant1 name ctor ( f1, m1 ) =
    objectVariant_ name
        [ ( f1, tsType m1 )
        ]
        (\c v1 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Json.Decode.map ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
        )


{-| Define a variant with 2 parameters for a custom type.
-}
namedVariant2 :
    String
    -> (a -> b -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> CustomCodec ((a -> b -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant2 name ctor ( f1, m1 ) ( f2, m2 ) =
    objectVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        ]
        (\c v1 v2 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Json.Decode.map2 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> TsEncode.UnionEncodeValue) a -> Codec a
buildCustom (CustomCodec am) =
    let
        discriminant : String
        discriminant =
            am.discriminant |> Maybe.withDefault "tag"

        decoder_ : Decoder a
        decoder_ =
            Json.Decode.field discriminant Json.Decode.string
                |> Json.Decode.andThen
                    (\tag ->
                        Dict.get tag am.decoder
                            |> Maybe.withDefault
                                (Json.Decode.fail <| discriminant ++ " \"" ++ tag ++ "\" did not match")
                    )

        encoder_ : Encoder a
        encoder_ =
            am.match |> TsEncode.buildUnion
    in
    TsJson.Internal.Codec.Codec
        { encoder = encoder_
        , decoder =
            TsJson.Internal.Decode.Decoder decoder_
                (TsEncode.tsType encoder_)
        }



-- INCONSISTENT STRUCTURE


{-| Try a set of decoders (in order).
The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.

This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
and the old ones (eventually `map`ped) as a fallback list to use while decoding.

-}
oneOf : Codec a -> List (Codec a) -> Codec a
oneOf main alts =
    Codec
        { encoder = encoder main
        , decoder = TsDecode.oneOf <| decoder main :: List.map decoder alts
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back codec =
    Codec
        { decoder = TsDecode.map go <| decoder codec
        , encoder = encoder codec |> TsEncode.map back
        }



-- FANCY


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` where you want to give a custom error message in some
case. The encoder will produce `null`.
-}
fail : String -> Codec a
fail msg =
    Codec
        { decoder = TsDecode.fail msg
        , encoder = TsEncode.null
        }


{-| Create a `Codec` for a recursive data structure.
The argument to the function you need to pass is the fully formed `Codec`.
-}
recursive : (Codec a -> Codec a) -> Codec a
recursive f =
    f <|
        lazy (\() -> recursive f)


{-| Create a `Codec` that produces null as JSON and always decodes as the same value.
-}
succeed : a -> Codec a
succeed default_ =
    Codec
        { decoder = TsDecode.succeed default_
        , encoder = TsEncode.null
        }


{-| This is useful for recursive structures that are not easily modeled with `recursive`.
Have a look at the Json.Decode docs for examples.
-}
lazy : (() -> Codec a) -> Codec a
lazy f =
    Codec
        { decoder =
            TsJson.Internal.Decode.Decoder
                (Json.Decode.lazy
                    (\() ->
                        decoder (f ())
                            |> TsDecode.decoder
                    )
                )
                TsType.Unknown
        , encoder =
            Encoder
                (\v -> (encoder (f ()) |> TsEncode.encoder) v)
                TsType.Unknown
        }


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Json.Decode.Value
value =
    Codec
        { encoder = TsEncode.value
        , decoder = TsDecode.value
        }


{-| -}
tsType : Codec value -> TsType
tsType (Codec thing) =
    TsEncode.tsType thing.encoder
