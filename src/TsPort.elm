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
    | Custom (List ( String, VariantTypeDef ))



--(List ( String, List TsType ))


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


custom :
    custom
    -> CustomBuilder custom
custom match =
    CustomBuilder match []


type CustomBuilder match
    = CustomBuilder match (List ( String, VariantTypeDef ))


type VariantTypeDef
    = Positional (List TsType)
    | KeyValue (List ( String, TsType ))


variant0 :
    String
    -> CustomBuilder (Encode.Value -> match)
    -> CustomBuilder match
variant0 variantName (CustomBuilder builder tsTypes_) =
    CustomBuilder
        (builder
            (Encode.object
                [ ( "type", Encode.string variantName ) ]
            )
        )
        (( variantName, Positional [] ) :: tsTypes_)


variant1 :
    String
    -> Encoder arg1
    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
    -> CustomBuilder match
variant1 variantName (Encoder encoder_ tsType_) (CustomBuilder builder tsTypes) =
    let
        mappedEncoder : arg1 -> Encode.Value
        mappedEncoder arg1 =
            Encode.object
                [ ( "type", Encode.string variantName )
                , ( "args"
                  , Encode.list identity
                        [ arg1 |> encoder_ ]
                  )
                ]
    in
    CustomBuilder
        (builder mappedEncoder)
        (( variantName, Positional [ tsType_ ] ) :: tsTypes)


objectVariant :
    String
    -> ObjectBuilder arg1
    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
    -> CustomBuilder match
objectVariant variantName (ObjectBuilder entries) (CustomBuilder builder tsTypes) =
    let
        objectTypeDef =
            entries
                |> List.map (\( key, encodeFn, tsType_ ) -> ( key, tsType_ ))

        --|> TypeObject
        mappedEncoder : arg1 -> Encode.Value
        mappedEncoder arg1 =
            Encode.object
                (( "type", Encode.string variantName )
                    :: (entries |> List.map (\( key, encodeFn, tsType_ ) -> ( key, encodeFn arg1 )))
                )
    in
    CustomBuilder
        (builder mappedEncoder)
        -- TODO need special Variant to allow different format
        (( variantName, KeyValue objectTypeDef ) :: tsTypes)


buildCustom : CustomBuilder (match -> Encode.Value) -> Encoder match
buildCustom (CustomBuilder toValue tsTypes_) =
    Encoder toValue (Custom tsTypes_)



--
--customVariantsToTsTypes : a -> List ( String, List TsType )
--customVariantsToTsTypes tsTypes_ =
--    Debug.todo ""
--Positional


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

        Custom tsTypes_ ->
            tsTypes_
                |> List.map
                    (\( variantName, variantTypes ) ->
                        case variantTypes of
                            Positional positionalArgs ->
                                "{ type : \""
                                    ++ variantName
                                    ++ "\"; "
                                    ++ argsToString positionalArgs
                                    ++ " }"

                            KeyValue keyValueArgs ->
                                "{ type : \""
                                    ++ variantName
                                    ++ "\"; "
                                    ++ keyValueArgsToString keyValueArgs
                                    ++ " }"
                    )
                |> String.join " | "


keyValueArgsToString : List ( String, TsType ) -> String
keyValueArgsToString keyValueArgs =
    List.map
        (\( key, tsType_ ) ->
            key ++ " : " ++ tsTypeToString tsType_
        )
        keyValueArgs
        |> String.join ""


argsToString : List TsType -> String
argsToString variantTypes =
    if List.isEmpty variantTypes then
        ""

    else
        "args: [ "
            ++ (List.map
                    (\tsType_ ->
                        tsTypeToString tsType_
                    )
                    variantTypes
                    |> String.join ""
               )
            ++ " ];"
