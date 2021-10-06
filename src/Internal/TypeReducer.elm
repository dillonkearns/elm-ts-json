module Internal.TypeReducer exposing
    ( intersect
    , union
    )

import Dict
import Dict.Extra
import Internal.JsonSchema
import Internal.TsJsonType exposing (..)
import Internal.TypeToString as TypeToString
import Json.Encode as Encode


deduplicateBy : (a -> comparable) -> List a -> List a
deduplicateBy toComparable list =
    List.foldl
        (\value accum -> Dict.insert (toComparable value) value accum)
        Dict.empty
        list
        |> Dict.values


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
    Dict.Extra.fromListDedupeBy
        (\( optionality1, fieldName1, fieldType1 ) ( optionality2, fieldName2, fieldType2 ) ->
            if optionality1 == Required || optionality2 == Required then
                ( Required, fieldName1, intersect fieldType1 fieldType2 )

            else
                ( Optional, fieldName1, fieldType1 )
        )
        (\( _, fieldName, _ ) -> fieldName)
        (fields1 ++ fields2)
        |> Dict.values


simplifyIntersection : List TsType -> TsType
simplifyIntersection types =
    let
        thing =
            case types |> deduplicateBy TypeToString.toString of
                [ single ] ->
                    single

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

    else if type1 == type2 then
        type1

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
                if type1 == type2 then
                    type1

                else
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

        Integer ->
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
