module TsPort exposing (..)

import Json.Encode as Encode


type Encoder encodesFrom
    = Encoder (encodesFrom -> Encode.Value) TsType


encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn tsType_) encodesFrom =
    encodeFn encodesFrom


typeDef : Encoder encodesFrom -> String
typeDef (Encoder encodeFn tsType_) =
    tsTypeToString tsType_


type ObjectBuilder encodesFrom
    = ObjectBuilder (List ( String, encodesFrom -> Encode.Value, TsType ))


type TsType
    = String
    | List TsType
    | TypeObject (List ( String, TsType ))
    | Custom


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
    Encoder Encode.string String


map : (encodesFrom -> value) -> Encoder value -> Encoder encodesFrom
map getter (Encoder encodeFn tsType_) =
    Encoder (\value -> value |> getter |> encodeFn) tsType_


list : Encoder a -> Encoder (List a)
list (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.list encodeFn encodesFrom)
        (List tsType_)



--custom : a
--custom : (match -> Encode.Value) -> CustomBuilder match value
--custom : match -> CustomBuilder match ()


custom :
    --( Encode.Value -> match)
    --(Encode.Value -> custom -> Encode.Value)
    custom
    -> CustomBuilder custom
custom match =
    --Debug.todo ""
    CustomBuilder match


type CustomBuilder match
    = CustomBuilder match


variant0 :
    String
    -> CustomBuilder (Encode.Value -> match)
    -> CustomBuilder match
variant0 variantName (CustomBuilder builder) =
    CustomBuilder
        (builder
            (Encode.object
                [ ( "type", Encode.string variantName ) ]
            )
        )


variant1 :
    String
    -> Encoder arg1
    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
    -> CustomBuilder match
variant1 variantName (Encoder encoder_ tsType_) (CustomBuilder builder) =
    CustomBuilder
        (encoder_ |> builder)



--(builder
--    (Encode.object
--        [ ( "type", Encode.string variantName ) ]
--    )
--)


buildCustom : CustomBuilder (match -> Encode.Value) -> Encoder match
buildCustom (CustomBuilder toValue) =
    Encoder toValue Custom


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
            |> TypeObject
        )


tsTypeToString : TsType -> String
tsTypeToString tsType =
    case tsType of
        String ->
            "string"

        List listType ->
            tsTypeToString listType ++ "[]"

        TypeObject keyTypes ->
            "{ "
                ++ (keyTypes
                        |> List.map
                            (\( key, tsType_ ) ->
                                key ++ " : " ++ tsTypeToString tsType_
                            )
                        |> String.join "; "
                   )
                ++ " }"

        Custom ->
            """{ type : "SendPresenceHeartbeat" }"""
