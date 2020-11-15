module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | List TsType
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | Union (List TsType)


type VariantTypeDef
    = Positional (List TsType)
    | KeyValue (List ( String, TsType ))


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
