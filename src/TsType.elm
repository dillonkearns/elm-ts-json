module TsType exposing
    ( PropertyOptionality(..)
    , TsType(..)
    , intersect
    , null
    , toString
    , union
    )

import Dict
import Json.Encode as Encode


deduplicateBy : (a -> comparable) -> List a -> List a
deduplicateBy toComparable list =
    List.foldl
        (\value accum -> Dict.insert (toComparable value) value accum)
        Dict.empty
        list
        |> Dict.values


type PropertyOptionality
    = Optional
    | Required


type TsType
    = String
    | Number
    | Boolean
    | List TsType
    | ArrayIndex ( Int, TsType ) (List ( Int, TsType ))
    | Tuple (List TsType) (Maybe TsType) -- Maybe is a rest type - can be used for non-empty lists https://stackoverflow.com/a/56006703
    | Literal Encode.Value
    | TypeObject (List ( PropertyOptionality, String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union ( TsType, List TsType )
    | Unknown
    | TsNever
    | Intersection (List TsType)


union : List TsType -> TsType
union tsTypes =
    let
        withoutNevers =
            tsTypes |> List.filter ((/=) TsNever)

        hadNevers =
            List.length tsTypes /= List.length withoutNevers
    in
    case withoutNevers of
        [ singleType ] ->
            singleType

        [] ->
            if hadNevers then
                TsNever

            else
                Unknown

        first :: rest ->
            Union ( first, rest )


mergeFields :
    List ( PropertyOptionality, String, TsType )
    -> List ( PropertyOptionality, String, TsType )
    -> List ( PropertyOptionality, String, TsType )
mergeFields fields1 fields2 =
    fields1 ++ fields2


simplifyIntersection : List TsType -> TsType
simplifyIntersection types =
    let
        thing =
            case types |> deduplicateBy toString of
                first :: rest ->
                    case first of
                        TypeObject fields ->
                            let
                                ( otherObjects, nonObjectTypes ) =
                                    List.foldr
                                        (\thisType ( objectsSoFar, otherSoFar ) ->
                                            case thisType of
                                                TypeObject theseFields ->
                                                    ( mergeFields theseFields objectsSoFar
                                                    , otherSoFar
                                                    )

                                                _ ->
                                                    ( objectsSoFar, thisType :: otherSoFar )
                                        )
                                        ( fields, [] )
                                        rest
                            in
                            Intersection
                                (TypeObject otherObjects
                                    :: nonObjectTypes
                                )

                        -- TODO intersect if there are others
                        --types |> Intersection
                        _ ->
                            types |> Intersection

                [] ->
                    TsNever
    in
    thing


intersect : TsType -> TsType -> TsType
intersect type1 type2 =
    if isContradictory ( type1, type2 ) then
        TsNever

    else
        case ( type1, type2 ) of
            ( Unknown, known ) ->
                known

            ( known, Unknown ) ->
                known

            ( Intersection types1, Intersection types2 ) ->
                simplifyIntersection (types1 ++ types2)

            ( ArrayIndex ( index1, indexType1 ) [], ArrayIndex ( index2, indexType2 ) [] ) ->
                ArrayIndex ( index1, indexType1 ) [ ( index2, indexType2 ) ]

            ( TypeObject fields1, TypeObject fields2 ) ->
                TypeObject (mergeFields fields1 fields2)

            ( TypeObject fields1, Union unionedTypes ) ->
                Intersection [ type1, type2 ]

            ( String, Number ) ->
                TsNever

            ( Number, String ) ->
                TsNever

            _ ->
                Intersection [ type1, type2 ]


either : (TsType -> Bool) -> ( TsType, TsType ) -> Bool
either predicateFn ( type1, type2 ) =
    predicateFn type1 || predicateFn type2


isContradictory : ( TsType, TsType ) -> Bool
isContradictory types =
    either isNonEmptyObject types && either isPrimitive types


isPrimitive : TsType -> Bool
isPrimitive tsType =
    case tsType of
        Number ->
            True

        String ->
            True

        Boolean ->
            True

        _ ->
            False


isNonEmptyObject : TsType -> Bool
isNonEmptyObject tsType =
    case tsType of
        TypeObject (atLeastOne :: possiblyMore) ->
            True

        _ ->
            False


null : TsType
null =
    Literal Encode.null


toString : TsType -> String
toString tsType_ =
    case tsType_ of
        -- leaf types
        TsNever ->
            "never"

        String ->
            "string"

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
