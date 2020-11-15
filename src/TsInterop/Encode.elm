module TsInterop.Encode exposing (..)

import Json.Encode as Encode
import TsType exposing (TsType)


type Encoder encodesFrom
    = Encoder (encodesFrom -> Encode.Value) TsType


encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn tsType_) encodesFrom =
    encodeFn encodesFrom


typeDef : Encoder encodesFrom -> String
typeDef (Encoder encodeFn tsType_) =
    TsType.tsTypeToString_ tsType_


rawType : ObjectBuilder a -> List ( String, TsType )
rawType (ObjectBuilder entries) =
    entries
        |> List.map (\( key, encodeFn, tsType_ ) -> ( key, tsType_ ))


type ObjectBuilder encodesFrom
    = ObjectBuilder (List ( String, encodesFrom -> Encode.Value, TsType ))


build : ObjectBuilder encodesFrom
build =
    ObjectBuilder []


property : String -> Encoder encodesFrom -> ObjectBuilder encodesFrom -> ObjectBuilder encodesFrom
property keyName (Encoder encodeFn tsType_) (ObjectBuilder entries) =
    ObjectBuilder
        (( keyName
         , encodeFn
         , tsType_
         )
            :: entries
        )


string : Encoder String
string =
    Encoder Encode.string TsType.String


map : (encodesFrom -> value) -> Encoder value -> Encoder encodesFrom
map getter (Encoder encodeFn tsType_) =
    Encoder (\value -> value |> getter |> encodeFn) tsType_


list : Encoder a -> Encoder (List a)
list (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.list encodeFn encodesFrom)
        (TsType.List tsType_)


custom :
    custom
    -> CustomBuilder custom
custom match =
    CustomBuilder match []


type CustomBuilder match
    = CustomBuilder match (List TsType)


variant0 :
    String
    -> CustomBuilder (Encode.Value -> match)
    -> CustomBuilder match
variant0 variantName (CustomBuilder builder tsTypes_) =
    CustomBuilder
        (builder
            (Encode.object
                [ ( "tag", Encode.string variantName ) ]
            )
        )
        (TsType.TypeObject [ ( "tag", TsType.Literal (Encode.string variantName) ) ]
            :: tsTypes_
        )



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


variantLiteral :
    Encode.Value
    -> CustomBuilder (Encode.Value -> match)
    -> CustomBuilder match
variantLiteral literalValue (CustomBuilder builder tsTypes) =
    CustomBuilder
        (builder literalValue)
        (TsType.Literal literalValue :: tsTypes)


objectVariant :
    String
    -> ObjectBuilder arg1
    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
    -> CustomBuilder match
objectVariant variantName (ObjectBuilder entries) (CustomBuilder builder tsTypes) =
    let
        objectTypeDef =
            ( "tag", TsType.Literal (Encode.string variantName) )
                :: (entries
                        |> List.map (\( key, encodeFn, tsType_ ) -> ( key, tsType_ ))
                   )

        mappedEncoder : arg1 -> Encode.Value
        mappedEncoder arg1 =
            Encode.object
                (( "tag", Encode.string variantName )
                    :: (entries
                            |> List.map
                                (\( key, encodeFn, tsType_ ) ->
                                    ( key, encodeFn arg1 )
                                )
                       )
                )
    in
    CustomBuilder
        (builder mappedEncoder)
        (TsType.TypeObject objectTypeDef :: tsTypes)


encodeProVariant :
    String
    -> ObjectBuilder arg1
    -> arg1
    -> Encode.Value
encodeProVariant variantName (ObjectBuilder entries) arg1 =
    Encode.object
        (( "tag", Encode.string variantName )
            :: (entries
                    |> List.map
                        (\( key, encodeFn, tsType_ ) ->
                            ( key, encodeFn arg1 )
                        )
               )
        )


type VariantBuilder
    = VariantBuilder


buildCustom : CustomBuilder (match -> Encode.Value) -> Encoder match
buildCustom (CustomBuilder toValue tsTypes_) =
    Encoder toValue (TsType.Union tsTypes_)


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


proTypeAnnotation : List ( String, List ( String, TsType ) ) -> String
proTypeAnnotation entries =
    customTypeDefToString entries


customTypeDefToString : List ( String, List ( String, TsType ) ) -> String
customTypeDefToString variants =
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
