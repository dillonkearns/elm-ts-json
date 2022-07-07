module Internal.TypeToString exposing
    ( quoteObjectKey
    , toString
    )

import Dict
import Internal.TsJsonType exposing (..)
import Json.Encode as Encode
import Regex exposing (Regex)


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
                |> List.map parenthesizeToString
                |> String.join " | "

        TypeObject keyTypes ->
            "{ "
                ++ (keyTypes
                        |> List.sortBy (\( _, fieldName, _ ) -> fieldName)
                        |> List.map
                            (\( optionality, key, tsType__ ) ->
                                let
                                    quotedKey =
                                        quoteObjectKey key
                                in
                                (case optionality of
                                    Required ->
                                        quotedKey

                                    Optional ->
                                        quotedKey ++ "?"
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
                |> List.map parenthesizeToString
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
        toString type_ |> parenthesize

    else
        toString type_


doubleQuote : String -> String
doubleQuote string =
    "\"" ++ string ++ "\""


identifierRegex : Regex
identifierRegex =
    Regex.fromString "^[a-zA-Z_][a-zA-Z0-9_]*$"
        |> Maybe.withDefault Regex.never


isIdentifier : String -> Bool
isIdentifier =
    Regex.find identifierRegex
        >> List.isEmpty


quoteObjectKey : String -> String
quoteObjectKey key =
    let
        needsQuotes =
            isIdentifier key
    in
    if needsQuotes then
        doubleQuote key

    else
        key
