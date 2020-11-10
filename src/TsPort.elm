module TsPort exposing (..)

import Json.Encode as Encode


type Encoder encodesFrom
    = Encoder (encodesFrom -> EncodeValue) TsType


encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn tsType_) encodesFrom =
    encodeFn encodesFrom
        |> serializeEncodeValue


serializeEncodeValue : EncodeValue -> Encode.Value
serializeEncodeValue value =
    case value of
        Object list_ ->
            list_
                |> List.map
                    (\( objectKey, objectValue, tsType ) ->
                        ( objectKey, objectValue )
                    )
                |> Encode.object


typeDef : Encoder encodesFrom -> String
typeDef (Encoder encodeFn tsType_) =
    tsTypeToString tsType_


type EncodeValue
    = Object (List ( String, Encode.Value, TsType ))


type ObjectBuilder encodesFrom
    = ObjectBuilder (List ( String, encodesFrom -> Encode.Value, TsType ))


type TsType
    = String
    | List TsType
    | TypeObject (List ( String, TsType ))


build : ObjectBuilder encodesFrom
build =
    ObjectBuilder []


string : String -> (value -> String) -> ObjectBuilder value -> ObjectBuilder value
string keyName getter (ObjectBuilder entries) =
    ObjectBuilder
        (( keyName
         , \encodesFrom -> Encode.string (getter encodesFrom)
         , String
         )
            :: entries
        )


list : String -> (value -> List String) -> ObjectBuilder value -> ObjectBuilder value
list keyName getter (ObjectBuilder entries) =
    ObjectBuilder
        (( keyName
         , \encodesFrom -> Encode.list Encode.string (getter encodesFrom)
         , String
         )
            :: entries
        )


personEncoder : Encoder { first : String, last : String }
personEncoder =
    build
        |> string "first" .first
        |> string "last" .last
        |> toEncoder


toEncoder : ObjectBuilder value -> Encoder value
toEncoder (ObjectBuilder entries) =
    Encoder
        (\encodesFrom ->
            entries
                |> List.map (\( key, encodeFn, tsType_ ) -> ( key, encodeFn encodesFrom, tsType_ ))
                |> Object
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
            --TODO
            "[]"

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
