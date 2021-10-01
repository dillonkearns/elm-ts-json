module TsJson.Codec exposing
    ( Codec, Value, Error
    , Decoder, decoder, decodeString, decodeValue
    , encoder, encodeToString, encodeToValue
    , string, bool, int, float
    , maybe, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, maybeField, nullableField, buildObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , oneOf
    , map
    , succeed, recursive, fail, lazy, value, build
    , tsType
    )

{-| A `Codec a` contain a JSON `Decoder a` and the corresponding `a -> Value` encoder.


# Definition

@docs Codec, Value, Error


# Decode

@docs Decoder, decoder, decodeString, decodeValue


# Encode

@docs encoder, encodeToString, encodeToValue


# Primitives

@docs string, bool, int, float


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, maybeField, nullableField, buildObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


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
import Json.Decode
import Json.Encode
import Set exposing (Set)
import TsJson.Decode as JD
import TsJson.Encode as JE exposing (Encoder, Property)
import TsJson.Internal.Decode
import TsJson.Internal.Encode exposing (Encoder(..))



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a
    = Codec
        { encoder : JE.Encoder a
        , decoder : Decoder a
        }


{-| Represents a JavaScript value.
-}
type alias Value =
    Json.Encode.Value


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
-}
type alias Error =
    Json.Decode.Error



-- DECODE


{-| A value that knows how to decode JSON values.
-}
type alias Decoder a =
    JD.Decoder a


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Decoder a
decoder (Codec m) =
    m.decoder


{-| Parse the given string into a JSON value and then run the `Codec` on it.
This will fail if the string is not well-formed JSON or if the `Codec`
fails for some reason.
-}
decodeString : Codec a -> String -> Result Error a
decodeString codec =
    Json.Decode.decodeString (decoder codec |> JD.decoder)


{-| Run a `Codec` to decode some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Codec a -> Value -> Result Error a
decodeValue codec =
    Json.Decode.decodeValue (decoder codec |> JD.decoder)



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> JE.Encoder a
encoder (Codec m) =
    m.encoder


{-| Convert a value into a prettified JSON string. The first argument specifies
the amount of indentation in the result string.
-}
encodeToString : Int -> Codec a -> (a -> String)
encodeToString indentation codec =
    encodeToValue codec >> Json.Encode.encode indentation


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Value
encodeToValue codec =
    codec
        |> encoder
        |> JE.encoder



-- BASE


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : JE.Encoder a -> Decoder a -> Codec a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    build JE.string JD.string


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build JE.bool JD.bool


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    build JE.int JD.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    build JE.float JD.float



-- DATA STRUCTURES


composite : (JE.Encoder b -> JE.Encoder a) -> (Decoder b -> Decoder a) -> Codec b -> Codec a
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
        { decoder = JD.maybe <| decoder codec
        , encoder = JE.maybe <| encoder codec
        }


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list =
    composite JE.list JD.list


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array =
    composite JE.array JD.array


{-| `Codec` between a JSON object and an Elm `Dict`.
-}
dict : Codec a -> Codec (Dict String a)
dict =
    composite
        (JE.dict identity)
        JD.dict


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set =
    composite
        (JE.map Set.toList << JE.list)
        (JD.map Set.fromList << JD.list)


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            JE.tuple
                (encoder m1)
                (encoder m2)
        , decoder =
            JD.tuple
                (decoder m1)
                (decoder m2)
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            JE.triple
                (encoder m1)
                (encoder m2)
                (encoder m3)
        , decoder =
            JD.triple
                (decoder m1)
                (decoder m2)
                (decoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom
        (\ferr fok v ->
            case v of
                Err err ->
                    ferr err

                Ok ok ->
                    fok ok
        )
        |> variant1 "Err" Err errorCodec
        |> variant1 "Ok" Ok valueCodec
        |> buildCustom



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : List (Property a)
        , decoder : Decoder b
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
        , decoder = JD.succeed ctor
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            JE.required name getter (encoder codec)
                :: ocodec.encoder
        , decoder = JD.map2 (\f x -> f x) ocodec.decoder (JD.field name (decoder codec))
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
            JE.optional name getter (encoder codec)
                :: ocodec.encoder
        , decoder =
            decoder codec
                |> JD.field name
                |> JD.maybe
                |> JD.map2 (\f x -> f x) ocodec.decoder
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
                |> JE.object
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : JE.UnionBuilder match
        , decoder : List (Decoder v)
        }


{-| Starts building a `Codec` for a custom type.

You need to pass a pattern matching function, built like this:

    type Semaphore
        = Red Int String
        | Yellow Float
        | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\red yellow green value ->
                case value of
                    Red i s ->
                        red i s

                    Yellow f ->
                        yellow f

                    Green ->
                        green
            )
            |> Codec.variant2 "Red" Red Codec.int Codec.string
            |> Codec.variant1 "Yellow" Yellow Codec.float
            |> Codec.variant0 "Green" Green
            |> Codec.buildCustom

-}
custom : match -> CustomCodec match value
custom match =
    CustomCodec
        { match = JE.union match
        , decoder = []
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    String
    -> decodesTo
    -> CustomCodec (JE.UnionEncodeValue -> input) decodesTo
    -> CustomCodec input decodesTo
variant0 name constructor codec =
    variant_ name
        []
        (\encodeCustomTypeArgs ->
            []
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
        )
        (Json.Decode.succeed constructor
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 0 parameters for a custom type.
-}
variant1 :
    String
    -> (arg1 -> v)
    -> Codec arg1
    -> CustomCodec ((arg1 -> JE.UnionEncodeValue) -> c) v
    -> CustomCodec c v
variant1 name constructor arg1Codec codec =
    variant_ name
        [ tsType arg1Codec
        ]
        (\encodeCustomTypeArgs a ->
            [ a |> JE.encoder (encoder arg1Codec)
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
        )
        (Json.Decode.map constructor
            (variantArgDecoder 0 arg1Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    String
    -> (arg1 -> arg2 -> v)
    -> Codec arg1
    -> Codec arg2
    -> CustomCodec ((arg1 -> arg2 -> JE.UnionEncodeValue) -> c) v
    -> CustomCodec c v
variant2 name constructor arg1Codec arg2Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 ->
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
        )
        (Json.Decode.map2 constructor
            (variantArgDecoder 0 arg1Codec)
            (variantArgDecoder 1 arg2Codec)
            |> variantArgsDecoder name
        )
        codec


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    String
    -> (arg1 -> arg2 -> arg3 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant3 name constructor arg1Codec arg2Codec arg3Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 ->
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
variant4 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant4 name constructor arg1Codec arg2Codec arg3Codec arg4Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 ->
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            , JE.encoder (encoder arg4Codec) arg4
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
variant5 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant5 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 ->
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            , JE.encoder (encoder arg4Codec) arg4
            , JE.encoder (encoder arg5Codec) arg5
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
variant6 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> Codec arg6
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant6 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec codec =
    variant_ name
        [ tsType arg1Codec
        , tsType arg2Codec
        , tsType arg3Codec
        , tsType arg4Codec
        , tsType arg5Codec
        , tsType arg6Codec
        ]
        (\encodeCustomTypeArgs arg1 arg2 arg3 arg4 arg5 arg6 ->
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            , JE.encoder (encoder arg4Codec) arg4
            , JE.encoder (encoder arg5Codec) arg5
            , JE.encoder (encoder arg6Codec) arg6
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
variant7 :
    String
    -> (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> v)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> Codec arg6
    -> Codec arg7
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant7 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec arg7Codec codec =
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
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            , JE.encoder (encoder arg4Codec) arg4
            , JE.encoder (encoder arg5Codec) arg5
            , JE.encoder (encoder arg6Codec) arg6
            , JE.encoder (encoder arg7Codec) arg7
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
variant8 :
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
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> arg7 -> arg8 -> JE.UnionEncodeValue) -> partial) v
    -> CustomCodec partial v
variant8 name constructor arg1Codec arg2Codec arg3Codec arg4Codec arg5Codec arg6Codec arg7Codec arg8Codec codec =
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
            [ JE.encoder (encoder arg1Codec) arg1
            , JE.encoder (encoder arg2Codec) arg2
            , JE.encoder (encoder arg3Codec) arg3
            , JE.encoder (encoder arg4Codec) arg4
            , JE.encoder (encoder arg5Codec) arg5
            , JE.encoder (encoder arg6Codec) arg6
            , JE.encoder (encoder arg7Codec) arg7
            , JE.encoder (encoder arg8Codec) arg8
            ]
                |> encodeCustomTypeArgs
                |> JE.UnionEncodeValue
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
    decoder codec |> JD.decoder |> Json.Decode.index index


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
    -> ((List Value -> Value) -> a)
    -> Json.Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        thing =
            JE.object
                [ --( "tag", JE.string name )
                  JE.required "tag" identity (JE.literal (Json.Encode.string name))
                , JE.required "args" identity (JE.list JE.value)

                --, JE.required "args" Tuple.pair JE.tuple
                ]

        enc =
            thing |> JE.encoder

        thisType =
            TsType.TypeObject
                [ ( TsType.Required, "tag", TsType.Literal (Json.Encode.string name) )
                , ( TsType.Required, "args", TsType.Tuple argTypes Nothing )
                ]
    in
    CustomCodec
        { match =
            case am.match of
                JE.UnionBuilder matcher types ->
                    JE.UnionBuilder (matcher (matchPiece enc))
                        --JE.tsType thing
                        (thisType :: types)

        --, decoder = Dict.insert name decoderPiece am.decoder
        , decoder =
            TsJson.Internal.Decode.Decoder decoderPiece thisType
                :: am.decoder
        }


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> JE.UnionEncodeValue) a -> Codec a
buildCustom (CustomCodec am) =
    Codec
        { encoder = am.match |> JE.buildUnion
        , decoder = JD.oneOf am.decoder
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
        , decoder = JD.oneOf <| decoder main :: List.map decoder alts
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back codec =
    Codec
        { decoder = JD.map go <| decoder codec
        , encoder = encoder codec |> JE.map back
        }



-- FANCY


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` where you want to give a custom error message in some
case. The encoder will produce `null`.
-}
fail : String -> Codec a
fail msg =
    Codec
        { decoder = JD.fail msg
        , encoder = JE.null
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
        { decoder = JD.succeed default_
        , encoder = JE.null
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
                            |> JD.decoder
                    )
                )
                --(tsType (f ()))
                TsType.Unknown
        , encoder =
            Encoder
                (\v -> (encoder (f ()) |> JE.encoder) v)
                TsType.Unknown
        }


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Value
value =
    Codec
        { encoder = JE.value
        , decoder = JD.value
        }


{-| -}
tsType : Codec value -> TsType
tsType (Codec thing) =
    JD.tsType thing.decoder
