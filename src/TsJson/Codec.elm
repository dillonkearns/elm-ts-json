module TsJson.Codec exposing
    ( Codec
    , decoder, encoder
    , string, bool, int, float
    , literal, stringLiteral
    , maybe, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, maybeField, nullableField, buildObject
    , stringUnion
    , CustomCodec, custom, buildCustom
    , variant0
    , namedVariant1, namedVariant2, namedVariant3, namedVariant4, namedVariant5, namedVariant6, namedVariant7, namedVariant8
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


# Literals

@docs literal, stringLiteral


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, maybeField, nullableField, buildObject


# Custom Types

@docs stringUnion

@docs CustomCodec, custom, buildCustom

@docs variant0


## Keyword Variants

@docs namedVariant1, namedVariant2, namedVariant3, namedVariant4, namedVariant5, namedVariant6, namedVariant7, namedVariant8


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
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
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
        , decoder : Dict String (Decode.Decoder v)
        , discriminant : Maybe String
        }


{-| Starts building a `Codec` for a custom type.

You need to pass a pattern matching function, built like this:

    import TsJson.Codec exposing (Codec)

    type Shape
        = Rectangle Int Int
        | Square Int
        | Circle Int

    shapeCodec : Codec Shape
    shapeCodec =
        Codec.custom (Just "shape")
            (\vRectangle vSquare vCircle shape ->
                case shape of
                    Rectangle width height ->
                        vRectangle width height

                    Square width ->
                        vSquare width

                    Circle radius ->
                        vCircle radius
            )
            |> Codec.namedVariant2 "rectangle" Rectangle ( "width", Codec.int ) ( "height", Codec.int )
            |> Codec.positionalVariant1 "square" Square Codec.int
            |> Codec.namedVariant1 "circle" Circle ( "radius", Codec.int )
            |> Codec.buildCustom

    The `TsType` for `shapeCodec` is the following discriminated union:

    ```typescript
    | { shape: "rectangle"; width: number; height: number }
    | { shape: "square"; args: [ number ] }
    | { shape: "circle"; radius: number }
    ```

-}
custom : Maybe String -> match -> CustomCodec match value
custom discriminant match =
    CustomCodec
        { match = TsEncode.union match
        , decoder = Dict.empty
        , discriminant = discriminant
        }


{-| Simple one-to-one mapping of Elm values to TypeScript strings (no arguments, just like values like an enumeration).

    import TsJson.Codec exposing (Codec)

    type DarkMode
        = Dark
        | Light

    darkModeCodec : Codec DarkMode
    darkModeCodec =
        Codec.stringUnion [ ( "dark", Dark ), ( "light", Light ) ]

The `TsType` for `darkModeCodec` is the following union:

```typescript
"dark" | "light"
```

-}
stringUnion : List ( String, value ) -> Codec value
stringUnion mappings =
    let
        unionDecoder : TsDecode.Decoder value
        unionDecoder =
            TsDecode.stringUnion mappings
    in
    TsJson.Internal.Codec.Codec
        { encoder =
            Encoder
                (\decoded ->
                    case find mappings decoded of
                        Just gotValue ->
                            gotValue |> Encode.string

                        Nothing ->
                            Encode.null
                )
                (TsDecode.tsType unionDecoder)
        , decoder = unionDecoder
        }


{-| -}
literal : value -> Encode.Value -> Codec value
literal mappedValue literalValue =
    Codec
        { encoder = TsEncode.literal literalValue
        , decoder = TsDecode.literal mappedValue literalValue
        }


{-| -}
stringLiteral : value -> String -> Codec value
stringLiteral mappedValue literalValue =
    Codec
        { encoder = TsEncode.literal (Encode.string literalValue)
        , decoder = TsDecode.stringLiteral mappedValue literalValue
        }


find : List ( String, value ) -> value -> Maybe String
find mappings mappedValue =
    case mappings of
        ( key, mapping ) :: rest ->
            if mapping == mappedValue then
                Just key

            else
                find rest mappedValue

        [] ->
            Nothing


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    String
    -> v
    -> CustomCodec (TsEncode.UnionEncodeValue -> c) v
    -> CustomCodec c v
variant0 name ctor =
    namedVariant_ name
        []
        (\c -> c [])
        (Decode.succeed ctor)


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
        (Decode.map constructor
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
        (Decode.map2 constructor
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
        (Decode.map3 constructor
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
        (Decode.map4 constructor
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
        (Decode.map5 constructor
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
        (Decode.map6 constructor
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
        (Decode.map7 constructor
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
        (Decode.map8 constructor
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


variantArgDecoder : Int -> Codec a -> Decode.Decoder a
variantArgDecoder index codec =
    decoder codec |> TsDecode.decoder |> Decode.index index


variantArgsDecoder : String -> Decode.Decoder a -> Decode.Decoder a
variantArgsDecoder expectedTagName argsDecoder =
    Decode.map2 (\() v -> v)
        (Decode.string
            |> Decode.andThen
                (\tagName ->
                    if expectedTagName == tagName then
                        Decode.succeed ()

                    else
                        Decode.fail ("Expected the following tag: " ++ expectedTagName)
                )
            |> Decode.field "tag"
        )
        (Decode.field "args" argsDecoder)


variant_ :
    String
    -> List TsType
    -> ((List Decode.Value -> Decode.Value) -> a)
    -> Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        discriminant =
            am.discriminant |> Maybe.withDefault "tag"

        thing =
            TsEncode.object
                [ TsEncode.required discriminant identity (TsEncode.literal (Encode.string name))
                , TsEncode.required "args" identity (TsEncode.list TsEncode.value)
                ]

        enc : List Encode.Value -> Encode.Value
        enc =
            thing |> TsEncode.encoder

        thisType =
            TsType.TypeObject
                [ ( TsType.Required, discriminant, TsType.Literal (Encode.string name) )
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


namedVariant_ :
    String
    -> List ( String, TsType )
    -> ((List ( String, TsEncode.UnionEncodeValue ) -> TsEncode.UnionEncodeValue) -> a)
    -> Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
namedVariant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        discriminant =
            am.discriminant |> Maybe.withDefault "tag"

        combine : List ( String, UnionEncodeValue ) -> TsEncode.Encoder (List ( String, UnionEncodeValue ))
        combine things =
            TsJson.Internal.Encode.Encoder
                (\_ ->
                    Encode.object
                        (( discriminant, Encode.string name ) :: List.map (Tuple.mapSecond unwrapped) things)
                )
                thisType

        unwrapped : UnionEncodeValue -> Encode.Value
        unwrapped (UnionEncodeValue rawValue) =
            rawValue

        enc : List ( String, UnionEncodeValue ) -> UnionEncodeValue
        enc props =
            props |> (combine props |> TsEncode.encoder) |> TsJson.Internal.Encode.UnionEncodeValue

        thisType : TsType
        thisType =
            TsType.TypeObject
                (( TsType.Required, discriminant, TsType.Literal (Encode.string name) )
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
    namedVariant_ name
        [ ( f1, tsType m1 )
        ]
        (\c v1 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
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
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        ]
        (\c v1 v2 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map2 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
        )


{-| Define a variant with 3 parameters for a custom type.
-}
namedVariant3 :
    String
    -> (a1 -> a2 -> a3 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> CustomCodec ((a1 -> a2 -> a3 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant3 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        ]
        (\c v1 v2 v3 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map3 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
        )


{-| Define a variant with 4 parameters for a custom type.
-}
namedVariant4 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> CustomCodec ((a1 -> a2 -> a3 -> a4 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant4 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        , ( f4, tsType m4 )
        ]
        (\c v1 v2 v3 v4 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map4 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Decode.field f4 <| TsDecode.decoder (decoder m4))
        )


{-| Define a variant with 5 parameters for a custom type.
-}
namedVariant5 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> CustomCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant5 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        , ( f4, tsType m4 )
        , ( f5, tsType m5 )
        ]
        (\c v1 v2 v3 v4 v5 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f5, TsEncode.encoder (encoder m5) v5 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map5 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Decode.field f5 <| TsDecode.decoder (decoder m5))
        )


{-| Define a variant with 6 parameters for a custom type.
-}
namedVariant6 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> ( String, Codec a6 )
    -> CustomCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant6 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        , ( f4, tsType m4 )
        , ( f5, tsType m5 )
        , ( f6, tsType m6 )
        ]
        (\c v1 v2 v3 v4 v5 v6 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f5, TsEncode.encoder (encoder m5) v5 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f6, TsEncode.encoder (encoder m6) v6 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map6 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Decode.field f6 <| TsDecode.decoder (decoder m6))
        )


{-| Define a variant with 7 parameters for a custom type.
-}
namedVariant7 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> ( String, Codec a6 )
    -> ( String, Codec a7 )
    -> CustomCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant7 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        , ( f4, tsType m4 )
        , ( f5, tsType m5 )
        , ( f6, tsType m6 )
        , ( f7, tsType m7 )
        ]
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f5, TsEncode.encoder (encoder m5) v5 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f6, TsEncode.encoder (encoder m6) v6 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f7, TsEncode.encoder (encoder m7) v7 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map7 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Decode.field f6 <| TsDecode.decoder (decoder m6))
            (Decode.field f7 <| TsDecode.decoder (decoder m7))
        )


{-| Define a variant with 8 parameters for a custom type.
-}
namedVariant8 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> ( String, Codec a6 )
    -> ( String, Codec a7 )
    -> ( String, Codec a8 )
    -> CustomCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomCodec c v
namedVariant8 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) ( f8, m8 ) =
    namedVariant_ name
        [ ( f1, tsType m1 )
        , ( f2, tsType m2 )
        , ( f3, tsType m3 )
        , ( f4, tsType m4 )
        , ( f5, tsType m5 )
        , ( f6, tsType m6 )
        , ( f7, tsType m7 )
        , ( f8, tsType m8 )
        ]
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f5, TsEncode.encoder (encoder m5) v5 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f6, TsEncode.encoder (encoder m6) v6 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f7, TsEncode.encoder (encoder m7) v7 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f8, TsEncode.encoder (encoder m8) v8 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Decode.map8 ctor
            (Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Decode.field f6 <| TsDecode.decoder (decoder m6))
            (Decode.field f7 <| TsDecode.decoder (decoder m7))
            (Decode.field f8 <| TsDecode.decoder (decoder m8))
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
            Decode.field discriminant Decode.string
                |> Decode.andThen
                    (\tag ->
                        Dict.get tag am.decoder
                            |> Maybe.withDefault
                                (Decode.fail <| discriminant ++ " \"" ++ tag ++ "\" did not match")
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
                (Decode.lazy
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
value : Codec Decode.Value
value =
    Codec
        { encoder = TsEncode.value
        , decoder = TsDecode.value
        }


{-| -}
tsType : Codec value -> TsType
tsType (Codec thing) =
    TsEncode.tsType thing.encoder
