module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | Boolean
    | List TsType
    | Tuple (List TsType)
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union (List TsType)
    | Unknown
    | TsNever


combine : TsType -> TsType -> TsType
combine type1 type2 =
    case ( type1, type2 ) of
        ( TypeObject fields1, TypeObject fields2 ) ->
            TypeObject (fields1 ++ fields2)

        ( TypeObject fields1, ObjectWithUniformValues valueType ) ->
            Debug.todo ""

        ( TypeObject fields1, _ ) ->
            TsNever

        _ ->
            type1


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

        Tuple tsTypes ->
            "[ "
                ++ (tsTypes
                        |> List.map
                            (\type_ ->
                                tsTypeToString_ type_
                            )
                        |> String.join ", "
                   )
                ++ " ]"

        TsNever ->
            "never"
