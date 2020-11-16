module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | Boolean
    | List TsType
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union (List TsType)
    | Unknown


null : TsType
null =
    Literal Encode.null


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

        Number ->
            "number"

        Boolean ->
            "boolean"

        ObjectWithUniformValues tsType ->
            "{ [key: string]: " ++ tsTypeToString_ tsType ++ " }"

        Unknown ->
            "unknown"
