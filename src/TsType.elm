module TsType exposing (..)

import Json.Encode as Encode


type TsType
    = String
    | Number
    | Boolean
    | List TsType
    | ArrayIndex Int TsType
    | Tuple (List TsType) (Maybe TsType) -- Maybe is a rest type - can be used for non-empty lists https://stackoverflow.com/a/56006703
    | Literal Encode.Value
    | TypeObject (List ( String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union ( TsType, List TsType )
    | Unknown
    | TsNever
    | Intersection (List TsType)


union : List TsType -> TsType
union tsTypes =
    case tsTypes |> List.filter ((/=) TsNever) of
        [ singleType ] ->
            singleType

        [] ->
            Unknown

        first :: rest ->
            Union ( first, rest )


combine : TsType -> TsType -> TsType
combine type1 type2 =
    case ( type1, type2 ) of
        ( TypeObject fields1, TypeObject fields2 ) ->
            TypeObject (fields1 ++ fields2)

        ( TypeObject fields1, ObjectWithUniformValues valueType ) ->
            Debug.todo ""

        ( TypeObject fields1, Union unionedTypes ) ->
            Intersection [ type1, type2 ]

        ( TypeObject fields1, _ ) ->
            TsNever

        ( String, Number ) ->
            TsNever

        ( Number, String ) ->
            TsNever

        _ ->
            Intersection [ type1, type2 ]


null : TsType
null =
    Literal Encode.null


tsTypeToString_ : TsType -> String
tsTypeToString_ tsType_ =
    case tsType_ of
        String ->
            "string"

        List listType ->
            parenthesizeToString listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue

        Union ( firstType, tsTypes ) ->
            (firstType :: tsTypes)
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

        Intersection types ->
            types
                |> List.map tsTypeToString_
                |> String.join " & "
                |> parenthesize

        ArrayIndex index tsType ->
            "["
                ++ ((List.repeat index "unknown"
                        ++ [ tsTypeToString_ tsType, "...unknown[]" ]
                    )
                        |> String.join ","
                   )
                ++ "]"


parenthesize : String -> String
parenthesize string =
    "(" ++ string ++ ")"


parenthesizeToString : TsType -> String
parenthesizeToString type_ =
    let
        needsParens =
            case type_ of
                Union types ->
                    True

                _ ->
                    False
    in
    if needsParens then
        "(" ++ tsTypeToString_ type_ ++ ")"

    else
        tsTypeToString_ type_
