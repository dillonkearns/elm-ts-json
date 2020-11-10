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


string : (encodesFrom -> String) -> Encoder encodesFrom
string getter =
    Encoder
        (\encodesFrom -> Encode.string (getter encodesFrom))
        String



--stringNew : Encoder ( encodesFrom -> String)


stringNew : Encoder String
stringNew =
    Encoder Encode.string String


map : (encodesFrom -> value) -> Encoder value -> Encoder encodesFrom
map getter (Encoder encodeFn tsType_) =
    Encoder (\value -> value |> getter |> encodeFn) tsType_


list : Encoder encodesFrom -> (encodesFrom -> List a) -> Encoder encodesFrom
list (Encoder encodeFn tsType_) getter =
    let
        thing : a -> Encode.Value
        thing =
            Debug.todo ""
    in
    Encoder
        (\encodesFrom -> Encode.list thing (getter encodesFrom))
        (List tsType_)



--list : String -> (value -> List String) -> ObjectBuilder value -> ObjectBuilder value
--list keyName getter (ObjectBuilder entries) =
--    ObjectBuilder
--        (( keyName
--         , \encodesFrom -> Encode.list Encode.string (getter encodesFrom)
--         , String
--         )
--            :: entries
--        )


personEncoder : Encoder { first : String, last : String }
personEncoder =
    build
        |> property "first" (string .first)
        |> property "last" (string .last)
        |> toEncoder


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
