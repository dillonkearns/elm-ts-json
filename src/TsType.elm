module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | List TsType
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | Union (List TsType)
    | Null
    | Custom (List ( String, VariantTypeDef ))


type VariantTypeDef
    = Positional (List TsType)
    | KeyValue (List ( String, TsType ))


tsTypeToString_ : TsType -> String
tsTypeToString_ tsType_ =
    case tsType_ of
        String ->
            "string"

        List listType ->
            tsTypeToString_ listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue

        Union tsTypes ->
            tsTypes
                |> List.map tsTypeToString_
                |> String.join " | "

        Null ->
            "null"

        TypeObject keyTypes ->
            "{ "
                ++ (keyTypes
                        |> List.map
                            (\( key, tsType__ ) ->
                                key ++ " : " ++ tsTypeToString_ tsType__
                            )
                        |> String.join "; "
                   )
                ++ " }"

        Custom tsTypes_ ->
            customTypeDefToString tsTypes_

        Number ->
            "number"


customTypeDefToString : List ( String, VariantTypeDef ) -> String
customTypeDefToString tsTypes_ =
    tsTypes_
        |> List.map
            (\( variantName, variantTypes ) ->
                case variantTypes of
                    Positional positionalArgs ->
                        "{ tag : \""
                            ++ variantName
                            ++ "\"; "
                            ++ argsToString positionalArgs
                            ++ " }"

                    KeyValue keyValueArgs ->
                        "{ tag : \""
                            ++ variantName
                            ++ "\"; "
                            ++ keyValueArgsToString keyValueArgs
                            ++ " }"
             --Literal literalValue ->
             --    Encode.encode 0 literalValue
            )
        |> String.join " | "


keyValueArgsToString : List ( String, TsType ) -> String
keyValueArgsToString keyValueArgs =
    List.map
        (\( key, tsType_ ) ->
            key ++ " : " ++ tsTypeToString_ tsType_
        )
        keyValueArgs
        |> String.join "; "


argsToString : List TsType -> String
argsToString variantTypes =
    if List.isEmpty variantTypes then
        ""

    else
        "args: [ "
            ++ (List.map
                    (\tsType_ ->
                        tsTypeToString_ tsType_
                    )
                    variantTypes
                    |> String.join ""
               )
            ++ " ];"
