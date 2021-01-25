module Internal.JsonSchema exposing (toJsonSchema, toJsonSchemaTopLevel)

import Dict
import Internal.TsJsonType exposing (PropertyOptionality(..), TsType(..))
import Json.Encode as Encode
import List.NonEmpty


toJsonSchemaTopLevel : TsType -> Encode.Value
toJsonSchemaTopLevel tsType =
    ( "$schema", Encode.string "http://json-schema.org/draft-07/schema#" )
        :: toJsonSchemaHelp tsType
        |> Encode.object


toJsonSchema : TsType -> Encode.Value
toJsonSchema tsType =
    toJsonSchemaHelp tsType
        |> Encode.object


toJsonSchemaHelp : TsType -> List ( String, Encode.Value )
toJsonSchemaHelp tsType =
    case tsType of
        String ->
            [ ( "type", Encode.string "string" )
            ]

        Number ->
            [ ( "type", Encode.string "number" )
            ]

        Integer ->
            [ ( "type", Encode.string "integer" )
            ]

        List listType ->
            [ ( "type", Encode.string "array" )
            , ( "items", toJsonSchema listType )
            ]

        TypeObject properties ->
            let
                -- according to the json schema spec, we can only include
                -- the "required" field if it is non-empty
                -- <https://json-schema.org/understanding-json-schema/reference/object.html#required-properties>
                requiredProperties : Maybe (List.NonEmpty.NonEmpty String)
                requiredProperties =
                    properties
                        |> List.filterMap
                            (\( requiredProperty, name, propertyType ) ->
                                case requiredProperty of
                                    Required ->
                                        Just name

                                    Optional ->
                                        Nothing
                            )
                        |> List.NonEmpty.fromList
            in
            [ Just ( "type", Encode.string "object" )
            , Just
                ( "properties"
                , Encode.object
                    (properties
                        |> List.map
                            (\( requiredProperty, name, propertyType ) ->
                                ( name, toJsonSchema propertyType )
                            )
                    )
                )
            , requiredProperties
                |> Maybe.map List.NonEmpty.toList
                |> Maybe.map
                    (\requiredProps -> ( "required", Encode.list Encode.string requiredProps ))
            ]
                |> List.filterMap identity

        Boolean ->
            [ ( "type", Encode.string "boolean" )
            ]

        Unknown ->
            []

        Literal literalJson ->
            [ ( "const", literalJson ) ]

        Union nonEmptyTypes ->
            [ ( "anyOf"
              , nonEmptyTypes
                    |> List.NonEmpty.toList
                    |> Encode.list toJsonSchema
              )
            ]

        Tuple tupleTypes maybeRestType ->
            case maybeRestType of
                Just restType ->
                    [ ( "additionalItems", toJsonSchema restType )
                    , ( "items"
                      , Encode.list toJsonSchema tupleTypes
                      )
                    , ( "minItems", Encode.int (List.length tupleTypes) )
                    , ( "type", Encode.string "array" )
                    ]

                Nothing ->
                    [ ( "items"
                      , Encode.list toJsonSchema tupleTypes
                      )
                    , ( "maxItems", Encode.int (List.length tupleTypes) )
                    , ( "minItems", Encode.int (List.length tupleTypes) )
                    , ( "type", Encode.string "array" )
                    ]

        Intersection intersectionTypes ->
            [ ( "allOf"
              , intersectionTypes
                    |> Encode.list toJsonSchema
              )
            ]

        ObjectWithUniformValues objectValueType ->
            [ ( "additionalProperties", toJsonSchema objectValueType )
            , ( "type", Encode.string "object" )
            ]

        ArrayIndex first rest ->
            let
                dict =
                    Dict.fromList (first :: rest)

                highestIndex : Int
                highestIndex =
                    dict
                        |> Dict.keys
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            [ ( "additionalItems", toJsonSchema Unknown )
            , ( "items"
              , (List.range 0 highestIndex
                    |> List.map
                        (\cur ->
                            Dict.get cur dict
                                |> Maybe.withDefault Unknown
                        )
                )
                    |> Encode.list toJsonSchema
              )
            , ( "minItems", Encode.int (highestIndex + 1) )
            , ( "type", Encode.string "array" )
            ]

        TsNever ->
            [ {- There may be a more direct way to
                 express this. This is just a roundabout
                 way of saying this can never happen.
                 Because a JSON value will never be both
                 a `string` and `boolean`, this is a
                 contradiction, and therefore equivalent
                 to marking a JSON value with a
                 TypeScript `never` type.
              -}
              ( "allOf"
              , Encode.list toJsonSchema [ String, Boolean ]
              )
            ]
