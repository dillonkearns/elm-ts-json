module TsJson.Codec.Advanced exposing (CustomObjectCodec, customObject, objectVariant0, objectVariant1, objectVariant2, objectVariant3, objectVariant4, objectVariant5, objectVariant6, objectVariant7, objectVariant8, buildCustomObject)

{-| Codecs that can encode/decode objects of a custom shape. These are similar to the codecs for custom types in the `Codec` module, but give you more control over the shape of the result.

@docs CustomObjectCodec, customObject, objectVariant0, objectVariant1, objectVariant2, objectVariant3, objectVariant4, objectVariant5, objectVariant6, objectVariant7, objectVariant8, buildCustomObject

-}

import Dict exposing (Dict)
import Internal.TsJsonType as TsType exposing (TsType)
import Json.Decode exposing (Decoder, Value)
import Json.Encode
import TsJson.Codec exposing (Codec, decoder, encoder)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import TsJson.Internal.Codec
import TsJson.Internal.Decode
import TsJson.Internal.Encode exposing (UnionBuilder(..))


{-| A partially built `Codec` for a [discriminated union object](https://basarat.gitbook.io/typescript/type-system/discriminated-unions).
-}
type CustomObjectCodec match v
    = CustomCodec
        { tagField : String
        , match : TsEncode.UnionBuilder match
        , decoder : Dict String (Json.Decode.Decoder v)
        }


{-| Starts building a `Codec` for an object with a custom object.

You need to pass the field name there the variant name will be stored, and a pattern matching function, built like this:

    import TsJson.Codec exposing (Codec)
    import TsJson.Codec.Advanced as Codec

    type Shape
        = Rectangle Int Int
        | Square Int
        | Circle Int

    shapeCodec : TsJson.Codec.Codec Shape
    shapeCodec =
        Codec.customObject "shape"
            (\vRectangle vSquare vCircle shape ->
                case shape of
                    Rectangle width height ->
                        vRectangle width height

                    Square width ->
                        vSquare width

                    Circle radius ->
                        vCircle radius
            )
            |> Codec.objectVariant2 "rectangle" Rectangle ( "width", TsJson.Codec.int ) ( "height", TsJson.Codec.int )
            |> Codec.objectVariant1 "square" Square ( "width", TsJson.Codec.int )
            |> Codec.objectVariant1 "circle" Circle ( "radius", TsJson.Codec.int )
            |> Codec.buildCustomObject

The `TsType` for `shapeCodec` is the following discriminated union:

```typescript
{ radius : number; shape : "circle" }
| { shape : "square"; width : number }
| { height : number; shape : "rectangle"; width : number }
```

-}
customObject : String -> match -> CustomObjectCodec match value
customObject tagField match =
    CustomCodec
        { tagField = tagField
        , match = TsJson.Internal.Encode.UnionBuilder match []
        , decoder = Dict.empty
        }


objectVariant_ :
    String
    -> List ( String, TsType )
    -> ((List ( String, TsEncode.UnionEncodeValue ) -> TsEncode.UnionEncodeValue) -> a)
    -> Json.Decode.Decoder v
    -> CustomObjectCodec (a -> b) v
    -> CustomObjectCodec b v
objectVariant_ name argTypes matchPiece decoderPiece (CustomCodec am) =
    let
        combine : List ( String, TsEncode.UnionEncodeValue ) -> TsEncode.Encoder (List ( String, TsEncode.UnionEncodeValue ))
        combine things =
            TsJson.Internal.Encode.Encoder
                (\_ ->
                    Json.Encode.object
                        (( am.tagField, Json.Encode.string name ) :: List.map (Tuple.mapSecond unwrapped) things)
                )
                thisType

        unwrapped : TsJson.Internal.Encode.UnionEncodeValue -> Json.Encode.Value
        unwrapped (TsJson.Internal.Encode.UnionEncodeValue rawValue) =
            rawValue

        enc : List ( String, TsEncode.UnionEncodeValue ) -> TsEncode.UnionEncodeValue
        enc props =
            props |> (combine props |> TsEncode.encoder) |> TsJson.Internal.Encode.UnionEncodeValue

        thisType : TsType
        thisType =
            TsType.TypeObject
                (( TsType.Required, am.tagField, TsType.Literal (Json.Encode.string name) )
                    :: List.map (\( argName, argType ) -> ( TsType.Required, argName, argType )) argTypes
                )
    in
    CustomCodec
        { tagField = am.tagField
        , match =
            case am.match of
                UnionBuilder matcher types ->
                    UnionBuilder (matcher (matchPiece enc))
                        (thisType :: types)
        , decoder = Dict.insert name decoderPiece am.decoder
        }


{-| Define a variant with 0 parameters for a custom type.
-}
objectVariant0 :
    String
    -> v
    -> CustomObjectCodec (TsEncode.UnionEncodeValue -> c) v
    -> CustomObjectCodec c v
objectVariant0 name ctor =
    objectVariant_ name
        []
        (\c -> c [])
        (Json.Decode.succeed ctor)


{-| Define a variant with 1 parameter for a custom type.
-}
objectVariant1 :
    String
    -> (a -> v)
    -> ( String, Codec a )
    -> CustomObjectCodec ((a -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant1 name ctor ( f1, m1 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
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
objectVariant2 :
    String
    -> (a -> b -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> CustomObjectCodec ((a -> b -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant2 name ctor ( f1, m1 ) ( f2, m2 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
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


{-| Define a variant with 3 parameters for a custom type.
-}
objectVariant3 :
    String
    -> (a1 -> a2 -> a3 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant3 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        ]
        (\c v1 v2 v3 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Json.Decode.map3 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
        )


{-| Define a variant with 4 parameters for a custom type.
-}
objectVariant4 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> a4 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant4 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        , ( f4, TsJson.Codec.tsType m4 )
        ]
        (\c v1 v2 v3 v4 ->
            c
                [ ( f1, TsEncode.encoder (encoder m1) v1 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f2, TsEncode.encoder (encoder m2) v2 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f3, TsEncode.encoder (encoder m3) v3 |> TsJson.Internal.Encode.UnionEncodeValue )
                , ( f4, TsEncode.encoder (encoder m4) v4 |> TsJson.Internal.Encode.UnionEncodeValue )
                ]
        )
        (Json.Decode.map4 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Json.Decode.field f4 <| TsDecode.decoder (decoder m4))
        )


{-| Define a variant with 5 parameters for a custom type.
-}
objectVariant5 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant5 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        , ( f4, TsJson.Codec.tsType m4 )
        , ( f5, TsJson.Codec.tsType m5 )
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
        (Json.Decode.map5 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Json.Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Json.Decode.field f5 <| TsDecode.decoder (decoder m5))
        )


{-| Define a variant with 6 parameters for a custom type.
-}
objectVariant6 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> ( String, Codec a6 )
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant6 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        , ( f4, TsJson.Codec.tsType m4 )
        , ( f5, TsJson.Codec.tsType m5 )
        , ( f6, TsJson.Codec.tsType m6 )
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
        (Json.Decode.map6 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Json.Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Json.Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Json.Decode.field f6 <| TsDecode.decoder (decoder m6))
        )


{-| Define a variant with 7 parameters for a custom type.
-}
objectVariant7 :
    String
    -> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> ( String, Codec a4 )
    -> ( String, Codec a5 )
    -> ( String, Codec a6 )
    -> ( String, Codec a7 )
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant7 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        , ( f4, TsJson.Codec.tsType m4 )
        , ( f5, TsJson.Codec.tsType m5 )
        , ( f6, TsJson.Codec.tsType m6 )
        , ( f7, TsJson.Codec.tsType m7 )
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
        (Json.Decode.map7 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Json.Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Json.Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Json.Decode.field f6 <| TsDecode.decoder (decoder m6))
            (Json.Decode.field f7 <| TsDecode.decoder (decoder m7))
        )


{-| Define a variant with 8 parameters for a custom type.
-}
objectVariant8 :
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
    -> CustomObjectCodec ((a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> TsEncode.UnionEncodeValue) -> c) v
    -> CustomObjectCodec c v
objectVariant8 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) ( f8, m8 ) =
    objectVariant_ name
        [ ( f1, TsJson.Codec.tsType m1 )
        , ( f2, TsJson.Codec.tsType m2 )
        , ( f3, TsJson.Codec.tsType m3 )
        , ( f4, TsJson.Codec.tsType m4 )
        , ( f5, TsJson.Codec.tsType m5 )
        , ( f6, TsJson.Codec.tsType m6 )
        , ( f7, TsJson.Codec.tsType m7 )
        , ( f8, TsJson.Codec.tsType m8 )
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
        (Json.Decode.map8 ctor
            (Json.Decode.field f1 <| TsDecode.decoder (decoder m1))
            (Json.Decode.field f2 <| TsDecode.decoder (decoder m2))
            (Json.Decode.field f3 <| TsDecode.decoder (decoder m3))
            (Json.Decode.field f4 <| TsDecode.decoder (decoder m4))
            (Json.Decode.field f5 <| TsDecode.decoder (decoder m5))
            (Json.Decode.field f6 <| TsDecode.decoder (decoder m6))
            (Json.Decode.field f7 <| TsDecode.decoder (decoder m7))
            (Json.Decode.field f8 <| TsDecode.decoder (decoder m8))
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustomObject : CustomObjectCodec (a -> TsEncode.UnionEncodeValue) a -> Codec a
buildCustomObject (CustomCodec am) =
    let
        decoder : Decoder a
        decoder =
            Json.Decode.field am.tagField Json.Decode.string
                |> Json.Decode.andThen
                    (\tag ->
                        Dict.get tag am.decoder
                            |> Maybe.withDefault
                                (Json.Decode.fail <| am.tagField ++ " \"" ++ tag ++ "\" did not match")
                    )

        encoder =
            am.match |> TsEncode.buildUnion
    in
    TsJson.Internal.Codec.Codec
        { encoder = encoder
        , decoder =
            TsJson.Internal.Decode.Decoder decoder
                (TsEncode.tsType encoder)
        }
