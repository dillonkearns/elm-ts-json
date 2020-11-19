module TsInterop.Decode exposing
    ( InteropDecoder
    , succeed
    , bool, float, int, string
    , field
    , list, array, nullable, oneOf, keyValuePairs, oneOrMore
    , map, map2, map3
    , literal
    , decoder, tsTypeToString
    )

{-|


## Decoders

@docs InteropDecoder

@docs succeed


## Built-Ins

@docs bool, float, int, string


## Objects

@docs field


## Composite Types

@docs list, array, nullable, oneOf, keyValuePairs, oneOrMore


## Transformations

@docs map, map2, map3


## TypeScript Literals

@docs literal


## Using Decoders

@docs decoder, tsTypeToString

-}

import Array exposing (Array)
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
nullable : InteropDecoder value -> InteropDecoder (Maybe value)
nullable (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.nullable innerDecoder) (Union [ innerType, TsType.null ])


{-| -}
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
            |> Union
        )


{-| -}
type InteropDecoder value
    = InteropDecoder (Decoder value) TsType


{-| -}
succeed : value -> InteropDecoder value
succeed value =
    InteropDecoder (Decode.succeed value) TsType.Unknown


{-| TypeScript has support for literals.
-}
literal : value -> Encode.Value -> InteropDecoder value
literal value literalValue =
    InteropDecoder
        (Decode.value
            |> Decode.andThen
                (\decodeValue ->
                    if literalValue == decodeValue then
                        Decode.succeed value

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
              ( field "tag" (literal () (Json.Encode.string "pass")) )
              ( field "message" string )
        ]

    oneOrMore (::) testCaseDecoder
        |> runExample """[ { "tag": "pass" } ]"""
    --> { decoded = Ok [ Pass ]
    --> , tsType = """[ { tag : "pass" } | { tag : "pass"; message : string }, ...({ tag : "pass" } | { tag : "pass"; message : string })[] ]"""
    --> }

-}
oneOrMore : (a -> List a -> value) -> InteropDecoder a -> InteropDecoder value
oneOrMore constructor (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.oneOrMore constructor innerDecoder) (Tuple [ innerType ] (Just innerType))


{-| -}
array : InteropDecoder value -> InteropDecoder (Array value)
array (InteropDecoder innerDecoder innerType) =
    InteropDecoder (Decode.array innerDecoder) (List innerType)


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


{-| -}
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
