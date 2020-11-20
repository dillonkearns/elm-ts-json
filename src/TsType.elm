module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | Boolean
    | List TsType
    | Tuple (List TsType) (Maybe TsType) -- Maybe is a rest type - can be used for non-empty lists https://stackoverflow.com/a/56006703
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union (List TsType)
    | Unknown
    | TsNever
    | Intersection TsType TsType


combine : TsType -> TsType -> TsType
combine type1 type2 =
    case ( type1, type2 ) of
        ( TypeObject fields1, TypeObject fields2 ) ->
            TypeObject (fields1 ++ fields2)

        ( TypeObject fields1, ObjectWithUniformValues valueType ) ->
            Debug.todo ""

        ( TypeObject fields1, Union unionedTypes ) ->
            Intersection type1 type2

        ( TypeObject fields1, _ ) ->
            TsNever

        ( String, Number ) ->
            TsNever

        ( Number, String ) ->
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

        Tuple tsTypes maybeRestType ->
            let
                restTypePart =
                    maybeRestType
                        |> Maybe.map
                            (\restType ->
                                "...(" ++ tsTypeToString_ restType ++ ")[]"
                            )
            in
            "[ "
                ++ (((tsTypes
                        |> List.map
                            (\type_ ->
                                tsTypeToString_ type_ |> Just
                            )
                     )
                        ++ [ restTypePart ]
                    )
                        |> List.filterMap identity
                        |> String.join ", "
                   )
                ++ " ]"

        TsNever ->
            "never"

        Intersection type1 type2 ->
            [ type1, type2 ]
                |> List.map tsTypeToString_
                |> String.join " & "
                |> parenthesize


parenthesize : String -> String
parenthesize string =
    "(" ++ string ++ ")"
