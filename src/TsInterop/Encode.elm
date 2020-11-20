module TsInterop.Encode exposing
    ( Encoder
    , string, int, float, literal, bool
    , typeDef, encoder
    , map
    , ObjectBuilder, build, property, buildUnion, toEncoder
    , UnionBuilder, union, variant, variant0, variantObject, variantLiteral
    , list, dict, tuple, triple
    , unionTypeDefToString, encodeProVariant, proTypeAnnotation, rawType, value
    , objectNew
    )

{-|

@docs Encoder


## Built-Ins

@docs string, int, float, literal, bool


## Executing Encoders

@docs typeDef, encoder


## Transforming

@docs map


## Objects

@docs ObjectBuilder, build, property, buildUnion, toEncoder


## Union Types

@docs UnionBuilder, union, variant, variant0, variantObject, variantLiteral


## Collections

@docs list, dict, tuple, triple


## Internal

@docs unionTypeDefToString, encodeProVariant, proTypeAnnotation, rawType, value

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import TsType exposing (TsType)


{-| -}
type Encoder encodesFrom
    = Encoder (encodesFrom -> Encode.Value) TsType


{-| -}
encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn tsType_) encodesFrom =
    encodeFn encodesFrom


{-| -}
typeDef : Encoder encodesFrom -> String
typeDef (Encoder encodeFn tsType_) =
    TsType.tsTypeToString_ tsType_


{-| -}
rawType : ObjectBuilder a -> List ( String, TsType )
rawType (ObjectBuilder entries) =
    entries
        |> List.map (\( key, encodeFn, tsType_ ) -> ( key, tsType_ ))


{-| -}
type ObjectBuilder encodesFrom
    = ObjectBuilder (List ( String, encodesFrom -> Encode.Value, TsType ))


{-| -}
objectNew : List ( String, Encoder value ) -> Encoder value
objectNew propertyEncoders =
    let
        propertyTypes : TsType
        propertyTypes =
            propertyEncoders
                |> List.map (Tuple.mapSecond (\(Encoder encodeFn tsType_) -> tsType_))
                |> TsType.TypeObject

        encodeObject : value -> Encode.Value
        encodeObject encodesFrom =
            propertyEncoders
                |> List.map
                    (Tuple.mapSecond
                        (\(Encoder encodeFn tsType_) -> encodeFn encodesFrom)
                    )
                |> Encode.object
    in
    Encoder encodeObject propertyTypes


{-| -}
build : ObjectBuilder encodesFrom
build =
    ObjectBuilder []


{-| -}
property : String -> Encoder encodesFrom -> ObjectBuilder encodesFrom -> ObjectBuilder encodesFrom
property keyName (Encoder encodeFn tsType_) (ObjectBuilder entries) =
    ObjectBuilder
        (( keyName
         , encodeFn
         , tsType_
         )
            :: entries
        )


{-| -}
bool : Encoder Bool
bool =
    Encoder Encode.bool TsType.Boolean


{-| -}
int : Encoder Int
int =
    Encoder Encode.int TsType.Number


{-| -}
float : Encoder Float
float =
    Encoder Encode.float TsType.Number


{-| -}
string : Encoder String
string =
    Encoder Encode.string TsType.String


{-| -}
literal : Encode.Value -> Encoder a
literal literalValue =
    Encoder (\_ -> literalValue) (TsType.Literal literalValue)


{-| -}
value : Encoder Encode.Value
value =
    Encoder identity TsType.Unknown


{-| -}
map : (encodesFrom -> value) -> Encoder value -> Encoder encodesFrom
map getter (Encoder encodeFn tsType_) =
    Encoder (\value_ -> value_ |> getter |> encodeFn) tsType_


{-| -}
list : Encoder a -> Encoder (List a)
list (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.list encodeFn encodesFrom)
        (TsType.List tsType_)


{-| -}
tuple : Encoder value1 -> Encoder value2 -> Encoder ( value1, value2 )
tuple (Encoder encodeFn1 tsType1) (Encoder encodeFn2 tsType2) =
    Encoder
        (\( value1, value2 ) ->
            Encode.list identity [ encodeFn1 value1, encodeFn2 value2 ]
        )
        (TsType.Tuple [ tsType1, tsType2 ] Nothing)


{-| -}
triple : Encoder value1 -> Encoder value2 -> Encoder value3 -> Encoder ( value1, value2, value3 )
triple (Encoder encodeFn1 tsType1) (Encoder encodeFn2 tsType2) (Encoder encodeFn3 tsType3) =
    Encoder
        (\( value1, value2, value3 ) ->
            Encode.list identity
                [ encodeFn1 value1
                , encodeFn2 value2
                , encodeFn3 value3
                ]
        )
        (TsType.Tuple [ tsType1, tsType2, tsType3 ] Nothing)


{-| -}
dict : (comparableKey -> String) -> Encoder value -> Encoder (Dict comparableKey value)
dict keyToString (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.dict keyToString encodeFn encodesFrom)
        (TsType.ObjectWithUniformValues tsType_)


{-| -}
union :
    constructor
    -> UnionBuilder constructor
union constructor =
    UnionBuilder constructor []


{-| -}
type UnionBuilder match
    = UnionBuilder match (List TsType)


{-| -}
variant0 :
    String
    -> UnionBuilder (Encode.Value -> match)
    -> UnionBuilder match
variant0 variantName (UnionBuilder builder tsTypes_) =
    let
        thing : UnionBuilder ((() -> Encode.Value) -> match)
        thing =
            UnionBuilder
                (builder
                    |> transformBuilder
                )
                tsTypes_

        transformBuilder : (Encode.Value -> match) -> (() -> Encode.Value) -> match
        transformBuilder matchBuilder encoderFn =
            matchBuilder (encoderFn ())
    in
    variant
        (build
            |> property "tag" (literal (Encode.string variantName))
            |> toEncoder
        )
        thing


{-| -}
variant :
    Encoder encodesFrom
    -> UnionBuilder ((encodesFrom -> Encode.Value) -> match)
    -> UnionBuilder match
variant (Encoder encoder_ tsType_) (UnionBuilder builder tsTypes_) =
    UnionBuilder
        (builder encoder_)
        (tsType_ :: tsTypes_)



--variant1 :
--    String
--    -> Encoder arg1
--    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
--    -> CustomBuilder match
--variant1 variantName (Encoder encoder_ tsType_) (CustomBuilder builder tsTypes) =
--    let
--        mappedEncoder : arg1 -> Encode.Value
--        mappedEncoder arg1 =
--            Encode.object
--                [ ( "tag", Encode.string variantName )
--                , ( "args"
--                  , Encode.list identity
--                        [ arg1 |> encoder_ ]
--                  )
--                ]
--    in
--    CustomBuilder
--        (builder mappedEncoder)
--        (
--        TypeObject [ ( "tag", Literal (Encode.string variantName) ) ]
--        ( variantName, Positional [ tsType_ ] ) :: tsTypes)


{-| -}
variantLiteral :
    Encode.Value
    -> UnionBuilder (Encode.Value -> match)
    -> UnionBuilder match
variantLiteral literalValue (UnionBuilder builder tsTypes) =
    UnionBuilder
        (builder literalValue)
        (TsType.Literal literalValue :: tsTypes)


{-| -}
variantObject :
    String
    -> List ( String, Encoder arg1 )
    -> UnionBuilder ((arg1 -> Encode.Value) -> match)
    -> UnionBuilder match
variantObject variantName objectFields unionBuilder =
    variant
        (objectNew (( "tag", literal (Encode.string variantName) ) :: objectFields))
        unionBuilder


{-| -}
encodeProVariant :
    String
    -> List ( String, Encoder arg1 )
    -> arg1
    -> Encode.Value
encodeProVariant variantName entries arg1 =
    arg1
        |> (objectNew
                (( "tag", literal (Encode.string variantName) )
                    :: entries
                )
                |> encoder
           )


{-| -}
buildUnion : UnionBuilder (match -> Encode.Value) -> Encoder match
buildUnion (UnionBuilder toValue tsTypes_) =
    Encoder toValue (TsType.Union tsTypes_)


{-| -}
toEncoder : ObjectBuilder value -> Encoder value
toEncoder (ObjectBuilder entries) =
    Encoder
        (\encodesFrom ->
            entries
                |> List.map
                    (\( key, encodeFn, tsType_ ) ->
                        ( key
                        , encodeFn encodesFrom
                        )
                    )
                |> Encode.object
        )
        (entries
            |> List.map (\( key, encodeFn, tsType_ ) -> ( key, tsType_ ))
            |> TsType.TypeObject
        )


{-| -}
proTypeAnnotation : List ( String, List ( String, TsType ) ) -> String
proTypeAnnotation entries =
    unionTypeDefToString entries


{-| -}
unionTypeDefToString : List ( String, List ( String, TsType ) ) -> String
unionTypeDefToString variants =
    variants
        |> List.map
            (\( variantName, objectProperties ) ->
                TsType.TypeObject
                    (( "tag", TsType.Literal (Encode.string variantName) )
                        :: objectProperties
                    )
            )
        |> TsType.Union
        |> TsType.tsTypeToString_
