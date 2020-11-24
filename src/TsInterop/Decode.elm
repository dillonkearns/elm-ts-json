module TsInterop.Decode exposing
    ( InteropDecoder
    , succeed, fail
    , bool, float, int, string
    , field, at
    , list, array, nullable, oneOf, dict, keyValuePairs, oneOrMore, maybe
    , index
    , map, map2
    , map3, map4, map5, map6, map7, map8
    , literal, null
    , andThen, staticAndThen, StaticAndThen, init, option
    , decoder, tsTypeToString
    )

{-|


## Decoders

@docs InteropDecoder

@docs succeed, fail


## Built-Ins

@docs bool, float, int, string


## Objects

@docs field, at


## Composite Types

@docs list, array, nullable, oneOf, dict, keyValuePairs, oneOrMore, maybe

@docs index


## Transformations

@docs map, map2

@docs map3, map4, map5, map6, map7, map8


## TypeScript Literals

@docs literal, null


## Continuation

@docs andThen, staticAndThen, StaticAndThen, init, option


## Using Decoders

@docs decoder, tsTypeToString

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import TsType exposing (TsType(..))


{-| -}
map : (value -> mapped) -> InteropDecoder value -> InteropDecoder mapped
map mapFn (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.map mapFn innerDecoder) innerType


{-| -}
map2 : (value1 -> value2 -> mapped) -> InteropDecoder value1 -> InteropDecoder value2 -> InteropDecoder mapped
map2 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) =
    InteropDecoder (Decode.map2 mapFn innerDecoder1 innerDecoder2) (TsType.combine innerType1 innerType2)


{-| -}
map3 : (value1 -> value2 -> value3 -> mapped) -> InteropDecoder value1 -> InteropDecoder value2 -> InteropDecoder value3 -> InteropDecoder mapped
map3 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) =
    InteropDecoder (Decode.map3 mapFn innerDecoder1 innerDecoder2 innerDecoder3)
        (TsType.combine
            innerType1
            innerType2
            |> TsType.combine innerType3
        )


{-| -}
map4 :
    (value1 -> value2 -> value3 -> value4 -> mapped)
    -> InteropDecoder value1
    -> InteropDecoder value2
    -> InteropDecoder value3
    -> InteropDecoder value4
    -> InteropDecoder mapped
map4 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) (InteropDecoder innerDecoder4 innerType4) =
    InteropDecoder (Decode.map4 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4)
        (TsType.Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            ]
        )


{-| -}
map5 :
    (value1 -> value2 -> value3 -> value4 -> value5 -> mapped)
    -> InteropDecoder value1
    -> InteropDecoder value2
    -> InteropDecoder value3
    -> InteropDecoder value4
    -> InteropDecoder value5
    -> InteropDecoder mapped
map5 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) (InteropDecoder innerDecoder4 innerType4) (InteropDecoder innerDecoder5 innerType5) =
    InteropDecoder (Decode.map5 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5)
        (TsType.Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            , innerType5
            ]
        )


{-| -}
map6 :
    (value1 -> value2 -> value3 -> value4 -> value5 -> value6 -> mapped)
    -> InteropDecoder value1
    -> InteropDecoder value2
    -> InteropDecoder value3
    -> InteropDecoder value4
    -> InteropDecoder value5
    -> InteropDecoder value6
    -> InteropDecoder mapped
map6 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) (InteropDecoder innerDecoder4 innerType4) (InteropDecoder innerDecoder5 innerType5) (InteropDecoder innerDecoder6 innerType6) =
    InteropDecoder (Decode.map6 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6)
        (TsType.Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            , innerType5
            , innerType6
            ]
        )


{-| -}
map7 :
    (value1 -> value2 -> value3 -> value4 -> value5 -> value6 -> value7 -> mapped)
    -> InteropDecoder value1
    -> InteropDecoder value2
    -> InteropDecoder value3
    -> InteropDecoder value4
    -> InteropDecoder value5
    -> InteropDecoder value6
    -> InteropDecoder value7
    -> InteropDecoder mapped
map7 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) (InteropDecoder innerDecoder4 innerType4) (InteropDecoder innerDecoder5 innerType5) (InteropDecoder innerDecoder6 innerType6) (InteropDecoder innerDecoder7 innerType7) =
    InteropDecoder (Decode.map7 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6 innerDecoder7)
        (TsType.Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            , innerType5
            , innerType6
            , innerType7
            ]
        )


{-| -}
map8 :
    (value1 -> value2 -> value3 -> value4 -> value5 -> value6 -> value7 -> value8 -> mapped)
    -> InteropDecoder value1
    -> InteropDecoder value2
    -> InteropDecoder value3
    -> InteropDecoder value4
    -> InteropDecoder value5
    -> InteropDecoder value6
    -> InteropDecoder value7
    -> InteropDecoder value8
    -> InteropDecoder mapped
map8 mapFn (InteropDecoder innerDecoder1 innerType1) (InteropDecoder innerDecoder2 innerType2) (InteropDecoder innerDecoder3 innerType3) (InteropDecoder innerDecoder4 innerType4) (InteropDecoder innerDecoder5 innerType5) (InteropDecoder innerDecoder6 innerType6) (InteropDecoder innerDecoder7 innerType7) (InteropDecoder innerDecoder8 innerType8) =
    InteropDecoder (Decode.map8 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6 innerDecoder7 innerDecoder8)
        (TsType.Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            , innerType5
            , innerType6
            , innerType7
            , innerType8
            ]
        )


{-| -}
nullable : InteropDecoder value -> InteropDecoder (Maybe value)
nullable (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.nullable innerDecoder) (TsType.union [ innerType, TsType.null ])


{-| You can express quite a bit with `oneOf`! The resulting TypeScript types will be a Union of all the TypeScript types
for each Decoder in the List.

    import Json.Decode
    import Json.Encode

    runExample : InteropDecoder value -> String -> { decoded : Result String value, tsType : String }
    runExample interopDecoder inputJson = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }


    "[1, 2, 3.14159, 4]"
        |> runExample ( list ( oneOf [ int |> map toFloat, float ] ) )
    --> { decoded = Ok [1.0, 2.0, 3.14159, 4.0]
    --> , tsType = """(number | number)[]"""
    --> }

-}
oneOf : List (InteropDecoder value) -> InteropDecoder value
oneOf decoders =
    InteropDecoder
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerDecoder
                )
            |> Decode.oneOf
        )
        (decoders
            |> List.map
                (\(InteropDecoder innerDecoder innerType) ->
                    innerType
                )
            |> TsType.union
        )


{-| -}
type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


{-| -}
succeed : value -> InteropDecoder value
succeed value_ =
    InteropDecoder (Decode.succeed value_) TsType.Unknown


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    field "version" int |> andThen (\versionNumber ->
        if versionNumber == 1 then
          field "payload" string
        else
          at [ "data", "payload" ] string
    )
        |> runExample """{"version": 1, "payload": "Hello"}"""
    --> { decoded = Ok "Hello"
    --> , tsType = "{ version : number }"
    --> }

-}
andThen : (a -> InteropDecoder b) -> InteropDecoder a -> InteropDecoder b
andThen function (InteropDecoder innerDecoder innerType) =
    let
        andThenDecoder =
            \value_ ->
                case function value_ of
                    InteropDecoder innerDecoder_ innerType_ ->
                        innerDecoder_
    in
    InteropDecoder (Decode.andThen andThenDecoder innerDecoder) innerType


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    example : StaticAndThen (Int -> InteropDecoder String)
    example =
        init
            (\v1Decoder v2PlusDecoder version ->
                case version of
                    1 -> v1Decoder
                    _ -> v2PlusDecoder
            )
            |> option (field "payload" string)
            |> option (at [ "data", "payload" ] string)


    field "version" int |> staticAndThen example
        |> runExample """{"version": 1, "payload": "Hello"}"""
    --> { decoded = Ok "Hello"
    --> , tsType = "({ version : number } & { data : { payload : string } } | { payload : string })"
    --> }

-}
staticAndThen : StaticAndThen (value -> InteropDecoder decodesTo) -> InteropDecoder value -> InteropDecoder decodesTo
staticAndThen (StaticAndThen function tsTypes) (InteropDecoder innerDecoder innerType) =
    let
        andThenDecoder =
            \value_ ->
                case function value_ of
                    InteropDecoder innerDecoder_ innerType_ ->
                        innerDecoder_
    in
    InteropDecoder (Decode.andThen andThenDecoder innerDecoder) (TsType.combine innerType (TsType.union tsTypes))


{-| -}
type StaticAndThen a
    = StaticAndThen a (List TsType)


{-| -}
init : a -> StaticAndThen a
init constructor =
    StaticAndThen constructor []


{-| -}
option :
    InteropDecoder value
    -> StaticAndThen (InteropDecoder value -> final)
    -> StaticAndThen final
option ((InteropDecoder innerDecoder innerType) as interopDecoder) (StaticAndThen function tsTypes) =
    StaticAndThen (function interopDecoder) (innerType :: tsTypes)


type IntOrString
    = TypeInt Int
    | TypeString String


example : StaticAndThen (Int -> InteropDecoder IntOrString)
example =
    init
        (\v1Decoder v2PlusDecoder version ->
            case version of
                1 ->
                    v1Decoder |> map TypeString

                _ ->
                    v2PlusDecoder |> map TypeInt
        )
        |> option string
        |> option int


{-| -}
value : InteropDecoder Decode.Value
value =
    InteropDecoder Decode.value TsType.Unknown


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    fail "Failure message"
        |> runExample "123.45"
    --> { decoded = Err "Problem with the given value:\n\n123.45\n\nFailure message"
    --> , tsType = "unknown"
    --> }

-}
fail : String -> InteropDecoder value
fail message =
    InteropDecoder (Decode.fail message) TsType.Unknown


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    null False |> runExample "null"
    --> { decoded = Ok False
    --> , tsType = "null"
    --> }

-}
null : value -> InteropDecoder value
null value_ =
    literal value_ Encode.null


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    json : String
    json = """{ "name": "tom", "age": 42 }"""

    maybe (field "height" float) |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ height : number } | unknown"
    --> }

    field "height" (maybe float) |> runExample json
    --> { decoded = Err "Problem with the given value:\n\n{\n        \"name\": \"tom\",\n        \"age\": 42\n    }\n\nExpecting an OBJECT with a field named `height`"
    --> , tsType = "{ height : number | unknown }"
    --> }

-}
maybe : InteropDecoder value -> InteropDecoder (Maybe value)
maybe interopDecoder =
    oneOf
        [ map Just interopDecoder
        , succeed Nothing
        ]


{-| TypeScript has support for literals.
-}
literal : value -> Encode.Value -> InteropDecoder value
literal value_ literalValue =
    InteropDecoder
        (Decode.value
            |> Decode.andThen
                (\decodeValue ->
                    if literalValue == decodeValue then
                        Decode.succeed value_

                    else
                        Decode.fail ("Expected the following literal value: " ++ Encode.encode 0 literalValue)
                )
        )
        (Literal literalValue)


{-|

    import Json.Decode

    runExample : InteropDecoder value -> String -> { decoded : Result String value, tsType : String }
    runExample interopDecoder inputJson = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    exampleDecoder : InteropDecoder String
    exampleDecoder =
        field "first" string

    """{"first":"James","middle":"Tiberius","last":"Kirk"}"""
        |> runExample exampleDecoder
    --> { decoded = Ok "James"
    --> , tsType = "{ first : string }"
    --> }

-}
field : String -> InteropDecoder value -> InteropDecoder value
field fieldName (InteropDecoder innerDecoder innerType) =
    InteropDecoder
        (Decode.field fieldName innerDecoder)
        (TsType.TypeObject [ ( fieldName, innerType ) ])


{-|

    import Json.Decode
    import Json.Encode

    runExample : InteropDecoder value -> String -> { decoded : Result String value, tsType : String }
    runExample interopDecoder inputJson = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    type Mode = DarkMode | LightMode

    modeDecoder : InteropDecoder Mode
    modeDecoder =
        oneOf [ literal DarkMode <| Json.Encode.string "dark", literal LightMode <| Json.Encode.string "light" ]

    """{"options":
           { "mode": "dark" },
        "version": "1.2.3"}"""
        |> runExample (at [ "options", "mode" ] modeDecoder)
    --> { decoded = Ok DarkMode
    --> , tsType = """{ options : { mode : "dark" | "light" } }"""
    --> }

-}
at : List String -> InteropDecoder value -> InteropDecoder value
at location (InteropDecoder innerDecoder innerType) =
    InteropDecoder
        (Decode.at location innerDecoder)
        (location
            |> List.foldr
                (\fieldName typeSoFar ->
                    TsType.TypeObject [ ( fieldName, typeSoFar ) ]
                )
                innerType
        )


{-| -}
string : InteropDecoder String
string =
    InteropDecoder Decode.string String


{-| -}
int : InteropDecoder Int
int =
    InteropDecoder Decode.int TsType.Number


{-| -}
float : InteropDecoder Float
float =
    InteropDecoder Decode.float TsType.Number


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    bool
        |> runExample "true"
    --> { decoded = Ok True
    --> , tsType = "boolean"
    --> }

-}
bool : InteropDecoder Bool
bool =
    InteropDecoder Decode.bool TsType.Boolean


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    list int
        |> runExample "[1,2,3]"
    --> { decoded = Ok [ 1, 2, 3 ]
    --> , tsType = "number[]"
    --> }

-}
list : InteropDecoder value -> InteropDecoder (List value)
list (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.list innerDecoder) (List innerType)


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    index 1 int
        |> runExample "[0,100,200]"
    --> { decoded = Ok 100
    --> , tsType = "[unknown,number,...unknown[]]"
    --> }

    map2 Tuple.pair
        ( index 1 int )
        ( index 3 string )
        |> runExample """[0,100,"a","b"]"""
    --> { decoded = Ok ( 100, "b" )
    --> , tsType = "([unknown,number,...unknown[]] & [unknown,unknown,unknown,string,...unknown[]])"
    --> }

-}
index : Int -> InteropDecoder value -> InteropDecoder value
index n (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.index n innerDecoder) (TsType.ArrayIndex n innerType)


{-|

    import Json.Decode
    import Json.Encode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    oneOrMore (::) int
        |> runExample "[12345]"
    --> { decoded = Ok [ 12345 ]
    --> , tsType = """[ number, ...(number)[] ]"""
    --> }

    type TestResult
        = Pass
        | Fail String

    testCaseDecoder : InteropDecoder TestResult
    testCaseDecoder =
        oneOf [
            field "tag" (literal Pass (Json.Encode.string "pass"))
          , map2 (\() message -> Fail message)
              ( field "tag" (literal () (Json.Encode.string "fail")) )
              ( field "message" string )
        ]

    oneOrMore (::) testCaseDecoder
        |> runExample """[ { "tag": "pass" } ]"""
    --> { decoded = Ok [ Pass ]
    --> , tsType = """[ { tag : "pass" } | { tag : "fail"; message : string }, ...({ tag : "pass" } | { tag : "fail"; message : string })[] ]"""
    --> }

-}
oneOrMore : (a -> List a -> value) -> InteropDecoder a -> InteropDecoder value
oneOrMore constructor (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.oneOrMore constructor innerDecoder) (Tuple [ innerType ] (Just innerType))


{-| Exactly the same as the `list` Decoder except that it wraps the decoded `List` into an Elm `Array`.
-}
array : InteropDecoder value -> InteropDecoder (Array value)
array (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.array innerDecoder) (List innerType)


{-|

    import Json.Decode
    import Json.Encode
    import Dict exposing (Dict)


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    dict int
        |> runExample """{"alice":42,"bob":99}"""
    --> { decoded = Ok (Dict.fromList [ ( "alice", 42 ), ( "bob", 99 ) ])
    --> , tsType = "{ [key: string]: number }"
    --> }

-}
dict : InteropDecoder value -> InteropDecoder (Dict String value)
dict (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.dict innerDecoder) (TsType.ObjectWithUniformValues innerType)


{-|

    import Json.Decode


    runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
    runExample inputJson interopDecoder = { tsType = tsTypeToString interopDecoder , decoded = Json.Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Json.Decode.errorToString }

    keyValuePairs int
        |> runExample """{ "alice": 42, "bob": 99 }"""
    --> { decoded = Ok [ ( "alice", 42 ), ( "bob", 99 ) ]
    --> , tsType = "number[]"
    --> }

-}
keyValuePairs : InteropDecoder value -> InteropDecoder (List ( String, value ))
keyValuePairs (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.keyValuePairs innerDecoder) (List innerType)


{-| Get a regular JSON Decoder that you can run using the `elm/json` API.
-}
decoder : InteropDecoder value -> Decoder value
decoder (InteropDecoder decoder_ tsType_) =
    decoder_


{-| -}
tsTypeToString : InteropDecoder value -> String
tsTypeToString (InteropDecoder decoder_ tsType_) =
    TsType.tsTypeToString_ tsType_


runExample : String -> InteropDecoder value -> { decoded : Result String value, tsType : String }
runExample inputJson interopDecoder =
    { tsType = tsTypeToString interopDecoder
    , decoded = Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Decode.errorToString
    }
