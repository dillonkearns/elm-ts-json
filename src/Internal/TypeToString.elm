module Internal.TypeToString exposing (toString)

import Dict
import Internal.TsJsonType exposing (..)
import Json.Encode as Encode


toString : TsType -> String
toString tsType_ =
    case tsType_ of
        -- leaf types
        TsNever ->
            "never"

        String ->
            "string"

        Integer ->
            "number"

        Number ->
            "number"

        Boolean ->
            "boolean"

        Unknown ->
            "JsonValue"

        -- compound types
        List listType ->
            parenthesizeToString listType ++ "[]"

        Literal literalValue ->
            Encode.encode 0 literalValue

        Union ( firstType, tsTypes ) ->
            (firstType :: tsTypes)
                |> List.map toString
                |> String.join " | "

        TypeObject keyTypes ->
            "{ "
                ++ (keyTypes
                        |> List.map
                            (\( optionality, key, tsType__ ) ->
                                (case optionality of
                                    Required ->
                                        key

                                    Optional ->
                                        key ++ "?"
                                )
                                    ++ " : "
                                    ++ toString tsType__
                            )
                        |> String.join "; "
                   )
                ++ " }"

        ObjectWithUniformValues tsType ->
            "{ [key: string]: " ++ toString tsType ++ " }"

        Tuple tsTypes maybeRestType ->
            let
                restTypePart =
                    maybeRestType
                        |> Maybe.map
                            (\restType ->
                                "...(" ++ toString restType ++ ")[]"
                            )
            in
            "[ "
                ++ (((tsTypes
                        |> List.map
                            (\type_ ->
                                toString type_ |> Just
                            )
                     )
                        ++ [ restTypePart ]
                    )
                        |> List.filterMap identity
                        |> String.join ", "
                   )
                ++ " ]"

        Intersection types ->
            types
                |> List.map toString
                |> String.join " & "
                |> parenthesize

        ArrayIndex ( index, tsType ) otherIndices ->
            let
                dict =
                    Dict.fromList
                        (( index, tsType )
                            :: otherIndices
                        )

                highestIndex : Int
                highestIndex =
                    dict
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            "["
                ++ (((List.range 0 highestIndex
                        |> List.map
                            (\cur ->
                                Dict.get cur dict
                                    |> Maybe.withDefault Unknown
                                    |> toString
                            )
                     )
                        ++ [ --tsTypeToString_ tsType,
                             "...JsonValue[]"
                           ]
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
        "(" ++ toString type_ ++ ")"

    else
        toString type_
