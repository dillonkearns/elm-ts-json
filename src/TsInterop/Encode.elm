module TsInterop.Encode exposing
    ( Encoder
    , string, int, float, literal, bool, null
    , typeDef, encoder
    , map
    , object, Property, optional, required
    , UnionBuilder, union, variant, variant0, variantObject, variantLiteral, buildUnion
    , UnionEncodeValue
    , list, dict, tuple, triple, maybe
    , value
    )

{-|

    import Json.Encode

    type Behavior
        = Auto
        | Smooth

    type Alignment
        = Start
        | Center
        | End
        | Nearest

    scrollIntoViewEncoder : Encoder
            { behavior : Maybe Behavior
            , block : Maybe Alignment
            , inline : Maybe Alignment
            }
    scrollIntoViewEncoder =
        object
            [ optional "behavior" .behavior behaviorEncoder
            , optional "block" .block alignmentEncoder
            , optional "inline" .inline alignmentEncoder
            ]

    behaviorEncoder : Encoder Behavior
    behaviorEncoder =
        union
            (\vAuto vSmooth value ->
                case value of
                    Auto ->
                        vAuto
                    Smooth ->
                        vSmooth
            )
            |> variantLiteral (Json.Encode.string "auto")
            |> variantLiteral (Json.Encode.string "smooth")
            |> buildUnion


    alignmentEncoder : Encoder Alignment
    alignmentEncoder =
        union
            (\vStart vCenter vEnd vNearest value ->
                case value of
                    Start ->
                        vStart
                    Center ->
                        vCenter
                    End ->
                        vEnd
                    Nearest ->
                        vNearest
            )
            |> variantLiteral (Json.Encode.string "start")
            |> variantLiteral (Json.Encode.string "center")
            |> variantLiteral (Json.Encode.string "end")
            |> variantLiteral (Json.Encode.string "nearest")
            |> buildUnion


    { behavior = Just Auto, block = Just Nearest, inline = Nothing }
            |> runExample scrollIntoViewEncoder
    --> { output = """{"behavior":"auto","block":"nearest"}"""
    --> , tsType = """{ behavior? : "smooth" | "auto"; block? : "nearest" | "end" | "center" | "start"; inline? : "nearest" | "end" | "center" | "start" }"""
    --> }

@docs Encoder


## Built-Ins

@docs string, int, float, literal, bool, null


## Executing Encoders

@docs typeDef, encoder


## Transforming

@docs map


## Objects

@docs object, Property, optional, required


## Union Types

    import Json.Encode as Encode

    runExample : Encoder encodeFrom -> encodeFrom -> { output : String, tsType : String }
    runExample encoder_ encodeFrom =
        { tsType = typeDef encoder_, output = encodeFrom |> encoder encoder_ |> Encode.encode 0 }

    type ToJs
        = SendPresenceHeartbeat
        | Alert String

    unionEncoder : Encoder ToJs
    unionEncoder =
        union
            (\vSendHeartbeat vAlert value ->
                case value of
                    SendPresenceHeartbeat ->
                        vSendHeartbeat
                    Alert string ->
                        vAlert string
            )
            |> variant0 "SendPresenceHeartbeat"
            |> variantObject "Alert" [ required "message" identity string ]
            |> buildUnion


    Alert "Hello TypeScript!"
            |> runExample unionEncoder
    --> { output = """{"tag":"Alert","message":"Hello TypeScript!"}"""
    --> , tsType = """{ tag : "Alert"; message : string } | { tag : "SendPresenceHeartbeat" }"""
    --> }

@docs UnionBuilder, union, variant, variant0, variantObject, variantLiteral, buildUnion

@docs UnionEncodeValue


## Collections

@docs list, dict, tuple, triple, maybe


## Escape Hatch

@docs value

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import TsType exposing (PropertyOptionality, TsType)


{-| -}
type Encoder encodesFrom
    = Encoder (encodesFrom -> Encode.Value) TsType


{-| -}
encoder : Encoder encodesFrom -> (encodesFrom -> Encode.Value)
encoder (Encoder encodeFn _) encodesFrom =
    encodeFn encodesFrom


{-| -}
typeDef : Encoder encodesFrom -> String
typeDef (Encoder _ tsType_) =
    TsType.toString tsType_


{-| -}
type Property encodesFrom
    = Property PropertyOptionality String (encodesFrom -> Maybe Encode.Value) TsType


{-| -}
optional : String -> (encodesFrom -> Maybe value) -> Encoder value -> Property encodesFrom
optional name getter (Encoder encodeFn tsType_) =
    Property
        TsType.Optional
        name
        (\encodesFrom -> encodesFrom |> getter |> Maybe.map encodeFn)
        tsType_


{-| -}
required : String -> (encodesFrom -> value) -> Encoder value -> Property encodesFrom
required name getter (Encoder encodeFn tsType_) =
    Property
        TsType.Required
        name
        (\encodesFrom -> encodesFrom |> getter |> encodeFn |> Just)
        tsType_


{-|

    import Json.Encode as Encode

    runExample : Encoder encodeFrom -> encodeFrom -> { output : String, tsType : String }
    runExample encoder_ encodeFrom =
        { tsType = typeDef encoder_, output = encodeFrom |> encoder encoder_ |> Encode.encode 0 }

    nameEncoder : Encoder { first : String, last : String }
    nameEncoder =
        object
            [ required "first" .first string
            , required "last" .last string
            ]


    { first = "James", last = "Kirk" }
            |> runExample nameEncoder
    --> { output = """{"first":"James","last":"Kirk"}"""
    --> , tsType = "{ first : string; last : string }"
    --> }

    fullNameEncoder : Encoder { first : String, middle : Maybe String, last : String }
    fullNameEncoder =
        object
            [ required "first" .first string
            , optional "middle" .middle string
            , required "last" .last string
            ]

    { first = "James", middle = Just "Tiberius", last = "Kirk" }
            |> runExample fullNameEncoder
    --> { output = """{"first":"James","middle":"Tiberius","last":"Kirk"}"""
    --> , tsType = "{ first : string; middle? : string; last : string }"
    --> }

-}
object : List (Property value) -> Encoder value
object propertyEncoders =
    let
        propertyTypes : TsType
        propertyTypes =
            propertyEncoders
                |> List.map
                    (\(Property optionality propertyName _ tsType_) ->
                        ( optionality, propertyName, tsType_ )
                    )
                |> TsType.TypeObject

        encodeObject : value -> Encode.Value
        encodeObject encodesFrom =
            propertyEncoders
                |> List.filterMap
                    (\(Property _ propertyName encodeFn _) ->
                        encodeFn encodesFrom
                            |> Maybe.map
                                (\encoded ->
                                    ( propertyName, encoded )
                                )
                    )
                |> Encode.object
    in
    Encoder encodeObject propertyTypes


{-| -}
bool : Encoder Bool
bool =
    Encoder Encode.bool TsType.Boolean


{-| -}
int : Encoder Int
int =
    Encoder Encode.int TsType.Number


{-| -}
float : Encoder Float
float =
    Encoder Encode.float TsType.Number


{-| Encode a string.

    import Json.Encode as Encode

    runExample : Encoder encodeFrom -> encodeFrom -> { output : String, tsType : String }
    runExample encoder_ encodeFrom = { tsType = typeDef encoder_ , output = encodeFrom |> encoder encoder_ |> Encode.encode 0 }


    "Hello!"
        |> runExample string
    --> { output = "\"Hello!\""
    --> , tsType = "string"
    --> }

You can use `map` to apply an accessor function for how to get that String.

    { data = { first = "James", last = "Kirk" } }
        |> runExample ( string |> map .first |> map .data )
    --> { output = "\"James\""
    --> , tsType = "string"
    --> }

-}
string : Encoder String
string =
    Encoder Encode.string TsType.String


{-| -}
literal : Encode.Value -> Encoder a
literal literalValue =
    Encoder (\_ -> literalValue) (TsType.Literal literalValue)


{-| Equivalent to `literal Encode.null`.

    import Json.Encode as Encode

    runExample : Encoder encodeFrom -> encodeFrom -> { output : String, tsType : String }
    runExample encoder_ encodeFrom = { tsType = typeDef encoder_ , output = encodeFrom |> encoder encoder_ |> Encode.encode 0 }


    ()
        |> runExample null
    --> { output = "null"
    --> , tsType = "null"
    --> }

-}
null : Encoder value
null =
    literal Encode.null


{-| This is an escape hatch that allows you to send arbitrary JSON data. The type will
be JSON in TypeScript, so you won't have any specific type information. In some cases,
this is fine, but in general you'll usually want to use other functions in this module
to build up a well-typed `Encoder`.
-}
value : Encoder Encode.Value
value =
    Encoder identity TsType.Unknown


{-| -}
map : (encodesFrom -> value) -> Encoder value -> Encoder encodesFrom
map getter (Encoder encodeFn tsType_) =
    Encoder (\value_ -> value_ |> getter |> encodeFn) tsType_


{-|

    import Json.Encode as Encode

    runExample : Encoder encodeFrom -> encodeFrom -> { output : String, tsType : String }
    runExample encoder_ encodeFrom = { tsType = typeDef encoder_ , output = encodeFrom |> encoder encoder_ |> Encode.encode 0 }


    Just 42
        |> runExample ( maybe int )
    --> { output = "42"
    --> , tsType = "number | null"
    --> }

-}
maybe : Encoder a -> Encoder (Maybe a)
maybe encoder_ =
    union
        (\vNull vJust maybeValue ->
            case maybeValue of
                Just justValue ->
                    vJust justValue

                Nothing ->
                    vNull
        )
        |> variantLiteral Encode.null
        |> variant encoder_
        |> buildUnion


{-| -}
list : Encoder a -> Encoder (List a)
list (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.list encodeFn encodesFrom)
        (TsType.List tsType_)


{-| -}
tuple : Encoder value1 -> Encoder value2 -> Encoder ( value1, value2 )
tuple (Encoder encodeFn1 tsType1) (Encoder encodeFn2 tsType2) =
    Encoder
        (\( value1, value2 ) ->
            Encode.list identity [ encodeFn1 value1, encodeFn2 value2 ]
        )
        (TsType.Tuple [ tsType1, tsType2 ] Nothing)


{-| -}
triple : Encoder value1 -> Encoder value2 -> Encoder value3 -> Encoder ( value1, value2, value3 )
triple (Encoder encodeFn1 tsType1) (Encoder encodeFn2 tsType2) (Encoder encodeFn3 tsType3) =
    Encoder
        (\( value1, value2, value3 ) ->
            Encode.list identity
                [ encodeFn1 value1
                , encodeFn2 value2
                , encodeFn3 value3
                ]
        )
        (TsType.Tuple [ tsType1, tsType2, tsType3 ] Nothing)


{-| -}
dict : (comparableKey -> String) -> Encoder value -> Encoder (Dict comparableKey value)
dict keyToString (Encoder encodeFn tsType_) =
    Encoder
        (\encodesFrom -> Encode.dict keyToString encodeFn encodesFrom)
        (TsType.ObjectWithUniformValues tsType_)


{-| -}
union :
    constructor
    -> UnionBuilder constructor
union constructor =
    UnionBuilder constructor []


{-| -}
type UnionBuilder match
    = UnionBuilder match (List TsType)


{-| -}
variant0 :
    String
    -> UnionBuilder (UnionEncodeValue -> match)
    -> UnionBuilder match
variant0 variantName (UnionBuilder builder tsTypes_) =
    let
        thing : UnionBuilder ((() -> UnionEncodeValue) -> match)
        thing =
            UnionBuilder
                (builder
                    |> transformBuilder
                )
                tsTypes_

        transformBuilder : (UnionEncodeValue -> match) -> (() -> UnionEncodeValue) -> match
        transformBuilder matchBuilder encoderFn =
            matchBuilder (encoderFn ())
    in
    variant
        (object
            [ required "tag" identity (literal (Encode.string variantName)) ]
        )
        thing


{-| -}
variant :
    Encoder encodesFrom
    -> UnionBuilder ((encodesFrom -> UnionEncodeValue) -> match)
    -> UnionBuilder match
variant (Encoder encoder_ tsType_) (UnionBuilder builder tsTypes_) =
    UnionBuilder
        (builder (encoder_ >> UnionEncodeValue))
        (tsType_ :: tsTypes_)



--variant1 :
--    String
--    -> Encoder arg1
--    -> CustomBuilder ((arg1 -> Encode.Value) -> match)
--    -> CustomBuilder match
--variant1 variantName (Encoder encoder_ tsType_) (CustomBuilder builder tsTypes) =
--    let
--        mappedEncoder : arg1 -> Encode.Value
--        mappedEncoder arg1 =
--            Encode.object
--                [ ( "tag", Encode.string variantName )
--                , ( "args"
--                  , Encode.list identity
--                        [ arg1 |> encoder_ ]
--                  )
--                ]
--    in
--    CustomBuilder
--        (builder mappedEncoder)
--        (
--        TypeObject [ ( "tag", Literal (Encode.string variantName) ) ]
--        ( variantName, Positional [ tsType_ ] ) :: tsTypes)


{-| -}
variantLiteral :
    Encode.Value
    -> UnionBuilder (UnionEncodeValue -> match)
    -> UnionBuilder match
variantLiteral literalValue (UnionBuilder builder tsTypes) =
    UnionBuilder
        (builder (literalValue |> UnionEncodeValue))
        (TsType.Literal literalValue :: tsTypes)


{-| -}
variantObject :
    String
    -> List (Property arg1)
    -> UnionBuilder ((arg1 -> UnionEncodeValue) -> match)
    -> UnionBuilder match
variantObject variantName objectFields unionBuilder =
    variant
        (object
            (required "tag" identity (literal (Encode.string variantName))
                :: objectFields
            )
        )
        unionBuilder


{-| We can guarantee that you're only encoding to a given
set of possible shapes in a union type by ensuring that
all the encoded values come from the union pipeline,
using functions like `variantLiteral`, `variantObject`, etc.

Applying another variant function in your union pipeline will
give you more functions/values to give UnionEncodeValue's with
different shapes, if you need them.

-}
type UnionEncodeValue
    = UnionEncodeValue Encode.Value


unwrapUnion : UnionEncodeValue -> Encode.Value
unwrapUnion (UnionEncodeValue rawValue) =
    rawValue


{-| -}
buildUnion : UnionBuilder (match -> UnionEncodeValue) -> Encoder match
buildUnion (UnionBuilder toValue tsTypes_) =
    Encoder (toValue >> unwrapUnion) (TsType.union tsTypes_)
