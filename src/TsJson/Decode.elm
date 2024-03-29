module TsJson.Decode exposing
    ( Decoder
    , succeed, fail
    , bool, float, int, string
    , field, at
    , list, array, nullable, oneOf, dict, keyValuePairs, oneOrMore, optionalField, optionalNullableField
    , index, tuple, triple
    , TupleBuilder, startTuple, buildTuple, element
    , map
    , map2, andMap
    , literal, null
    , stringLiteral, stringUnion
    , discriminatedUnion
    , andThen, AndThenContinuation, andThenInit, andThenDecoder
    , value, unknownAndThen, maybe
    , decoder, tsType
    , map3, map4, map5, map6, map7, map8
    , runExample
    )

{-| The `TsJson.Decode` module is what you use for

  - Flags
  - ToElm Ports

By building a Decoder with this API, you're also describing the source of truth for how to take a TypeScript type and
turn it into an Elm type. Note that there is complete type information here just by using this API (no magic parsing or
static analysis needed).

Let's take this example:

    import Json.Decode


    int
        |> map String.fromInt
        |> runExample "1000"
    --> { decoded = Ok "1000"
    --> , tsType = "number"
    --> }

In this example, there are no differences from how we would write this with the `elm/json` API. Let's consider the type
information from two different sides: the Elm types (which would be used in an Elm type annotation), and the TypeScript
types (which show up in the TypeScript Declaration file that `elm-ts-interop` generates to gives you nice autocompletion and type information).


### The Elm type information

  - The initial `Decoder`, `TsJson.Decode.int`, has the Elm type `TsJson.Decode.Decoder Int`. So Elm knows
    this `Decoder` will either fail, or give us an `Int`.
  - When we call `TsJson.Decode.map String.fromInt`, the Elm type information changes. We're mapping with
    `String.fromInt : Int -> String`. So that means we'll now decode into an Elm `String` instead of an `Int`. And that's
    the final Elm type we'll end up with.


### The TypeScript type information

  - `TsJson.Decode.int` expects a number from TypeScript.
  - `TsJson.Decode.map` applies a function to the decoded Elm value, but it doesn't change what we expect from TypeScript.
    So we still expect a `number` from TypeScript. And we're done, so that's the final type we expect to receive from TypeScript.


### Summary

We can apply more Decoders, like `TsJson.Decode.list`, for example, to expect an array of that type from TypeScript,
and a `List` of that Elm type. From there, it's just the same concept. **All the type information about the type that Elm will decode into, and the type that Elm expects from TypeScript, is built up as you build a Decoder**.

That means that the source of truth is the Decoder itself. Note that the Decoder doesn't specify just the Elm format, or just the TypeScript type as the source
of truth. It specifies **how to turn a TypeScript type into an Elm type as the source of truth**. That means that

  - You can change the decoder to change either type independently
  - The two sides (Elm and TS) will always be in sync (as long as you are re-running the `elm-ts-interop` CLI at the appropriate time)


## Decoders

@docs Decoder

@docs succeed, fail


## Built-Ins

@docs bool, float, int, string


## Objects

@docs field, at


## Composite Types

@docs list, array, nullable, oneOf, dict, keyValuePairs, oneOrMore, optionalField, optionalNullableField

@docs index, tuple, triple


## Arbitrary-Length Tuples

[TypeScript allows you to define a tuple](https://www.typescriptlang.org/docs/handbook/basic-types.html#tuple) that can
have any length of items with specific types.

TypeScript tuples are much like an Elm tuples, except two key differences:

  - Elm tuples can only have 2 or 3 items, while TypeScript tuples can have any length
  - Elm tuples are a distinct type, while TypeScript tuples are just arrays with a statically known length

@docs TupleBuilder, startTuple, buildTuple, element


## Transformations

@docs map


## Combining

@docs map2, andMap


## TypeScript Literals

@docs literal, null

@docs stringLiteral, stringUnion


## Discriminated Unions

@docs discriminatedUnion


## Continuation

@docs andThen, AndThenContinuation, andThenInit, andThenDecoder


## elm/json Decoder Escape Hatches

If you have an existing decoder, you can use it with an [`unknown` type](https://mariusschulz.com/blog/the-unknown-type-in-typescript) in TypeScript.

You can also decode an arbitrary JSON value as with `elm/json`, and then use `elm/json` to process it further.

@docs value, unknownAndThen, maybe


## Using Decoders

Usually you don't need to use these functions directly, but instead the code generated by the `elm-ts-interop` command line
tool will use these for you under the hood. These can be helpful for debugging, or for building new tools on top of this package.

@docs decoder, tsType


## mapN for Combining More Than Two Items

@docs map3, map4, map5, map6, map7, map8


## Internals

@docs runExample

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Internal.TsJsonType exposing (..)
import Internal.TypeReducer as TypeReducer
import Json.Decode as Decode
import Json.Encode as Encode
import TsJson.Internal.Decode exposing (Decoder(..))
import TsType


{-|

    import Json.Decode


    int
        |> map String.fromInt
        |> runExample "1000"
    --> { decoded = Ok "1000"
    --> , tsType = "number"
    --> }

-}
map : (value -> mapped) -> Decoder value -> Decoder mapped
map mapFn (Decoder innerDecoder innerType) =
    Decoder (Decode.map mapFn innerDecoder) innerType


{-| This is useful for building up a decoder with multiple fields in a pipeline style.
See <https://github.com/elm-community/json-extra/blob/2.0.0/docs/andMap.md>.

    import Json.Decode


    type alias Country = { name : String, populationInMillions : Int }

    succeed Country
        |> andMap (field "name" string)
        |> andMap (field "population" (int |> map (\totalPopulation -> floor (toFloat totalPopulation / 1000000.0))))
        |> runExample """ {"name": "Norway", "population":5328000} """
    --> { decoded = Ok { name = "Norway", populationInMillions = 5 }
    --> , tsType = "{ name : string; population : number }"
    --> }

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


{-| You can use [`map2`](#map2), [`map3`](#map3), etc. to build up decoders that have somewhat clearer error messages if something goes wrong.
Some people prefer the pipeline style using [`andMap`](#andMap) as it has fewer parentheses and you don't have to change the number when
you add a new field. It's a matter of personal preference.

    import Json.Decode


    type alias Country = { name : String, populationInMillions : Int }

    map2 Country
        (field "name" string)
        (field "population" (int |> map (\totalPopulation -> floor (toFloat totalPopulation / 1000000.0))))
        |> runExample """ {"name": "Norway", "population":5328000} """
    --> { decoded = Ok { name = "Norway", populationInMillions = 5 }
    --> , tsType = "{ name : string; population : number }"
    --> }

-}
map2 : (value1 -> value2 -> mapped) -> Decoder value1 -> Decoder value2 -> Decoder mapped
map2 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) =
    Decoder (Decode.map2 mapFn innerDecoder1 innerDecoder2) (TypeReducer.intersect innerType1 innerType2)


{-| -}
map3 : (value1 -> value2 -> value3 -> mapped) -> Decoder value1 -> Decoder value2 -> Decoder value3 -> Decoder mapped
map3 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) =
    Decoder (Decode.map3 mapFn innerDecoder1 innerDecoder2 innerDecoder3)
        (TypeReducer.intersect
            innerType1
            innerType2
            |> TypeReducer.intersect innerType3
        )


{-| -}
map4 :
    (value1 -> value2 -> value3 -> value4 -> mapped)
    -> Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder value4
    -> Decoder mapped
map4 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) (Decoder innerDecoder4 innerType4) =
    Decoder (Decode.map4 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4)
        (Intersection
            [ innerType1
            , innerType2
            , innerType3
            , innerType4
            ]
        )


{-| -}
map5 :
    (value1 -> value2 -> value3 -> value4 -> value5 -> mapped)
    -> Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder value4
    -> Decoder value5
    -> Decoder mapped
map5 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) (Decoder innerDecoder4 innerType4) (Decoder innerDecoder5 innerType5) =
    Decoder (Decode.map5 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5)
        (Intersection
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
    -> Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder value4
    -> Decoder value5
    -> Decoder value6
    -> Decoder mapped
map6 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) (Decoder innerDecoder4 innerType4) (Decoder innerDecoder5 innerType5) (Decoder innerDecoder6 innerType6) =
    Decoder (Decode.map6 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6)
        (Intersection
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
    -> Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder value4
    -> Decoder value5
    -> Decoder value6
    -> Decoder value7
    -> Decoder mapped
map7 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) (Decoder innerDecoder4 innerType4) (Decoder innerDecoder5 innerType5) (Decoder innerDecoder6 innerType6) (Decoder innerDecoder7 innerType7) =
    Decoder (Decode.map7 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6 innerDecoder7)
        (Intersection
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
    -> Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder value4
    -> Decoder value5
    -> Decoder value6
    -> Decoder value7
    -> Decoder value8
    -> Decoder mapped
map8 mapFn (Decoder innerDecoder1 innerType1) (Decoder innerDecoder2 innerType2) (Decoder innerDecoder3 innerType3) (Decoder innerDecoder4 innerType4) (Decoder innerDecoder5 innerType5) (Decoder innerDecoder6 innerType6) (Decoder innerDecoder7 innerType7) (Decoder innerDecoder8 innerType8) =
    Decoder (Decode.map8 mapFn innerDecoder1 innerDecoder2 innerDecoder3 innerDecoder4 innerDecoder5 innerDecoder6 innerDecoder7 innerDecoder8)
        (Intersection
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


{-|

    import Json.Decode


    nullable int |> runExample "13"
    --> { decoded = Ok (Just 13)
    --> , tsType = "number | null"
    --> }

    nullable int |> runExample "null"
    --> { decoded = Ok Nothing
    --> , tsType = "number | null"
    --> }

    nullable int |> runExample "true"
    --> { decoded = Err "Json.Decode.oneOf failed in the following 2 ways:\n\n\n\n(1) Problem with the given value:\n    \n    true\n    \n    Expecting null\n\n\n\n(2) Problem with the given value:\n    \n    true\n    \n    Expecting an INT"
    --> , tsType = "number | null"
    --> }

-}
nullable : Decoder value -> Decoder (Maybe value)
nullable (Decoder innerDecoder innerType) =
    Decoder (Decode.nullable innerDecoder) (TypeReducer.union [ innerType, Literal Encode.null ])


{-| You can express quite a bit with `oneOf`! The resulting TypeScript types will be a Union of all the TypeScript types
for each [`Decoder`](#Decoder) in the List.

    import Json.Decode
    import Json.Encode

    list ( oneOf [ int |> map toFloat, float ] )
        |> runExample "[1, 2, 3.14159, 4]"
    --> { decoded = Ok [1.0, 2.0, 3.14159, 4.0]
    --> , tsType = """(number | number)[]"""
    --> }

-}
oneOf : List (Decoder value) -> Decoder value
oneOf decoders =
    Decoder
        (decoders
            |> List.map
                (\(Decoder innerDecoder _) ->
                    innerDecoder
                )
            |> Decode.oneOf
        )
        (decoders
            |> List.map
                (\(Decoder _ innerType) ->
                    innerType
                )
            |> TypeReducer.union
        )


{-| Just like a `Decoder` in `elm/json`, except these `Decoder`s track the TypeScript types that they can successfully handle.
-}
type alias Decoder value =
    TsJson.Internal.Decode.Decoder value


{-|

    import Json.Decode


    succeed "abcdefg"
        |> runExample "12345"
    --> { decoded = Ok "abcdefg"
    --> , tsType = "JsonValue"
    --> }

-}
succeed : value -> Decoder value
succeed value_ =
    Decoder (Decode.succeed value_) Unknown


{-| If you need to run a regular JSON Decoder in an `andThen`, you can use this function, but it will yield a Decoder
with an unknown TypeScript type.

    import Json.Decode as JD


    field "version" int |> unknownAndThen (\versionNumber ->
        if versionNumber == 1 then
          JD.field "payload" JD.string
        else
          JD.at [ "data", "payload" ] JD.string
    )
        |> runExample """{"version": 1, "payload": "Hello"}"""
    --> { decoded = Ok "Hello"
    --> , tsType = "JsonValue"
    --> }

-}
unknownAndThen : (a -> Decode.Decoder b) -> Decoder a -> Decoder b
unknownAndThen function (Decoder innerDecoder innerType) =
    Decoder (Decode.andThen function innerDecoder) Unknown


{-|

    import Json.Decode


    example : AndThenContinuation (Int -> Decoder String)
    example =
        andThenInit
            (\v1Decoder v2PlusDecoder version ->
                case version of
                    1 -> v1Decoder
                    _ -> v2PlusDecoder
            )
            |> andThenDecoder (field "payload" string)
            |> andThenDecoder (at [ "data", "payload" ] string)


    field "version" int |> andThen example
        |> runExample """{"version": 1, "payload": "Hello"}"""
    --> { decoded = Ok "Hello"
    --> , tsType = "({ version : number } & ({ data : { payload : string } } | { payload : string }))"
    --> }

-}
andThen : AndThenContinuation (value -> Decoder decodesTo) -> Decoder value -> Decoder decodesTo
andThen (StaticAndThen function tsTypes) (Decoder innerDecoder innerType) =
    let
        andThenDecoder_ =
            \value_ ->
                case function value_ of
                    Decoder innerDecoder_ innerType_ ->
                        innerDecoder_
    in
    Decoder (Decode.andThen andThenDecoder_ innerDecoder) (TypeReducer.intersect innerType (TypeReducer.union tsTypes))


{-| Decode a TypeScript [Discriminated Union](https://www.typescriptlang.org/docs/handbook/2/narrowing.html#discriminated-unions)
with a String discriminant value. For example, if you wanted to decode something with the following TypeScript type:

```typescript
{ id : number; role : "admin" } | { role : "guest" }
```

You could use this Decoder:

    import TsJson.Decode as TsDecode

    type User
        = Admin { id : Int }
        | Guest


    TsDecode.discriminatedUnion "role"
        [ ( "admin"
          , TsDecode.succeed (\id -> Admin { id = id })
                |> TsDecode.andMap (TsDecode.field "id" TsDecode.int)
          )
        , ( "guest", TsDecode.succeed Guest )
        ]
        |> TsDecode.runExample """{"role": "admin", "id": 123}"""
    --> { decoded = Ok (Admin { id = 123 })
    --> , tsType = """{ id : number; role : "admin" } | { role : "guest" }"""
    --> }

-}
discriminatedUnion :
    String
    -> List ( String, Decoder decoded )
    -> Decoder decoded
discriminatedUnion discriminantField decoders =
    let
        table =
            Dict.fromList decoders
    in
    Decoder
        (Decode.field discriminantField Decode.string
            |> Decode.andThen
                (\discriminantValue ->
                    case Dict.get discriminantValue table of
                        Just variantDecoder ->
                            decoder variantDecoder

                        Nothing ->
                            Decode.fail <| "Unexpected discriminant value '" ++ discriminantValue ++ "' for field '" ++ discriminantField ++ "'"
                )
        )
        (decoders
            |> List.map
                (\( discriminantValue, variantDecoder ) ->
                    TypeReducer.intersect
                        (Internal.TsJsonType.TypeObject
                            [ ( Internal.TsJsonType.Required, discriminantField, Internal.TsJsonType.Literal (Encode.string discriminantValue) )
                            ]
                        )
                        (tsType variantDecoder)
                )
            |> TypeReducer.union
        )


{-| A convenience function for building a union out of string literals.

    import TsJson.Decode as TsDecode

    type Severity
        = Info
        | Warning
        | Error

    TsDecode.stringUnion
        [ ( "info", Info )
        , ( "warning", Warning )
        , ( "error", Error )
        ]
        |> TsDecode.runExample """ "info" """
    --> { decoded = Ok Info
    --> , tsType = "\"info\" | \"warning\" | \"error\""
    --> }

-}
stringUnion :
    List ( String, value )
    -> Decoder value
stringUnion unionMappings =
    Decoder
        (Decode.string
            |> Decode.andThen
                (\key ->
                    case unionMappings |> Dict.fromList |> Dict.get key of
                        Just mapped ->
                            Decode.succeed mapped

                        Nothing ->
                            Decode.fail <|
                                "I was expecting a string union with one of these string values: "
                                    ++ "[ "
                                    ++ (unionMappings
                                            |> List.map (\( mapKey, _ ) -> "\"" ++ mapKey ++ "\"")
                                            |> String.join ", "
                                       )
                                    ++ " ]"
                )
        )
        (TypeReducer.union
            (unionMappings
                |> List.map
                    (\( mapKey, _ ) ->
                        Literal (Encode.string mapKey)
                    )
            )
        )


{-| This type allows you to combine all the possible Decoders you could run in an [`andThen`](#andThen) continuation.

This API allows you to define all possible Decoders you might use up front, so that all possible TypeScript types
the continuation could decode are known _after building up the decoder instead of after running the decoder._

-}
type AndThenContinuation a
    = StaticAndThen a (List TsType)


{-| -}
andThenInit : a -> AndThenContinuation a
andThenInit constructor =
    StaticAndThen constructor []


{-| -}
andThenDecoder :
    Decoder value
    -> AndThenContinuation (Decoder value -> final)
    -> AndThenContinuation final
andThenDecoder ((Decoder _ innerType) as interopDecoder) (StaticAndThen function tsTypes) =
    StaticAndThen (function interopDecoder) (innerType :: tsTypes)


{-| Gives you an escape hatch to decode to a plain `elm/json` `Json.Decode.Value`. This has the TypeScript type `unknown`.
Avoid using this when possible.

    import Json.Decode


    value
        |> runExample "Hello"
    --> { decoded = (Json.Decode.decodeString Json.Decode.value "Hello" |> Result.mapError Json.Decode.errorToString)
    --> , tsType = "JsonValue"
    --> }

-}
value : Decoder Decode.Value
value =
    Decoder Decode.value Unknown


{-|

    import Json.Decode


    fail "Failure message"
        |> runExample "123.45"
    --> { decoded = Err "Problem with the given value:\n\n123.45\n\nFailure message"
    --> , tsType = "JsonValue"
    --> }

-}
fail : String -> Decoder value
fail message =
    Decoder (Decode.fail message) Unknown


{-|

    import Json.Decode


    null False
        |> runExample "null"
    --> { decoded = Ok False
    --> , tsType = "null"
    --> }

-}
null : value -> Decoder value
null value_ =
    literal value_ Encode.null


{-| This function is somewhat risky in that it could cover up a failing Decoder by turning it into a `Nothing`. In
some cases, this may be what you're looking for, but if you're trying to deal with optional fields, it's safer to use
[`optionalField`](#optionalField) and it will give you better type information. See the `thisShouldBeABoolean` example below. In that example,
we're decoding a JSON value which should have been a `boolean` but instead is a `string`. We'd like the `Decoder` to fail
to let us know it wasn't able to process the value correctly, but instead it covers up the failure and decodes to `Nothing`.

So use this `Decoder` with care!

    import Json.Decode


    json : String
    json = """{ "name": "tom", "age": 42, "thisShouldBeABoolean": "true" }"""


    -- WARNING: uh oh, this may not be the desired behavior!
    maybe (field "thisShouldBeABoolean" bool)
        |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ thisShouldBeABoolean : boolean } | JsonValue"
    --> }

    maybe (field "height" float)
        |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ height : number } | JsonValue"
    --> }

    field "height" (maybe float)
        |> runExample json
    --> { decoded = Err "Problem with the given value:\n\n{\n        \"name\": \"tom\",\n        \"age\": 42,\n        \"thisShouldBeABoolean\": \"true\"\n    }\n\nExpecting an OBJECT with a field named `height`"
    --> , tsType = "{ height : number | JsonValue }"
    --> }

-}
maybe : Decoder value -> Decoder (Maybe value)
maybe interopDecoder =
    oneOf
        [ map Just interopDecoder
        , succeed Nothing
        ]


{-| This is a safer (and more explicit) way to deal with optional fields compared to [`maybe`](#maybe). It may seem that wrapping
a [`field`](#field) `Decoder` in a [`maybe`](#maybe) `Decoder` achieves the same behavior, but the key difference is that the `maybe` version
will convert a failing decoder on a value that is present into `Nothing`, as if it wasn't present. Often what you want
is for the malformed version to fail, which is exactly what this function will do.

    import Json.Decode


    json : String
    json = """{ "name": "tom", "age": null }"""

    optionalField "height" float
        |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ height? : number }"
    --> }

    optionalField "age" int
        |> runExample json
    --> { decoded = Err "Problem with the value at json.age:\n\n    null\n\nExpecting an INT"
    --> , tsType = "{ age? : number }"
    --> }

-}
optionalField : String -> Decoder value -> Decoder (Maybe value)
optionalField fieldName (Decoder innerDecoder innerType) =
    let
        finishDecoding json =
            case Decode.decodeValue (Decode.field fieldName Decode.value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    Decode.map Just (Decode.field fieldName innerDecoder)

                Err _ ->
                    -- The field was missing, which is fine!
                    Decode.succeed Nothing
    in
    Decoder
        (Decode.value
            |> Decode.andThen finishDecoding
        )
        (TypeObject [ ( Optional, fieldName, innerType ) ])


{-|

    import Json.Decode


    json : String
    json = """{ "name": "tom", "age": null }"""

    optionalNullableField "height" float
        |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ height? : number | null }"
    --> }

    optionalNullableField "age" int
        |> runExample json
    --> { decoded = Ok Nothing
    --> , tsType = "{ age? : number | null }"
    --> }

-}
optionalNullableField : String -> Decoder value -> Decoder (Maybe value)
optionalNullableField fieldName interopDecoder =
    optionalField fieldName (nullable interopDecoder)
        |> map (Maybe.andThen identity)


{-| TypeScript has support for literals.

    import Json.Decode
    import Json.Encode as JE


    literal () (JE.string "unit")
        |> runExample """ "unit" """
    --> { decoded = Ok ()
    --> , tsType = "\"unit\""
    --> }

-}
literal : value -> Encode.Value -> Decoder value
literal value_ literalValue =
    Decoder
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


{-| A convenience function for building `literal (Json.Encode.string "my-literal-string")`.

    import TsJson.Decode as TsDecode


    TsDecode.stringLiteral () "unit"
        |> TsDecode.runExample """ "unit" """
    --> { decoded = Ok ()
    --> , tsType = "\"unit\""
    --> }

-}
stringLiteral : value -> String -> Decoder value
stringLiteral value_ stringLiteralValue =
    Decoder
        (Decode.string
            |> Decode.andThen
                (\decodeValue ->
                    if stringLiteralValue == decodeValue then
                        Decode.succeed value_

                    else
                        Decode.fail ("Expected the following string literal value: \"" ++ stringLiteralValue ++ "\"")
                )
        )
        (Literal (Encode.string stringLiteralValue))


{-|

    import Json.Decode


    field "first" string
        |> runExample """{"first":"James","middle":"Tiberius","last":"Kirk"}"""
    --> { decoded = Ok "James"
    --> , tsType = "{ first : string }"
    --> }

-}
field : String -> Decoder value -> Decoder value
field fieldName (Decoder innerDecoder innerType) =
    Decoder
        (Decode.field fieldName innerDecoder)
        (TypeObject [ ( Required, fieldName, innerType ) ])


{-|

    import Json.Decode
    import Json.Encode

    type Mode
        = DarkMode
        | LightMode

    modeDecoder : Decoder Mode
    modeDecoder =
        oneOf [ literal DarkMode <| Json.Encode.string "dark", literal LightMode <| Json.Encode.string "light" ]
            |> (at [ "options", "mode" ])

    modeDecoder
        |> runExample """{
                           "options": { "mode": "dark" },
                           "version": "1.2.3"
                         }"""
    --> { decoded = Ok DarkMode
    --> , tsType = """{ options : { mode : "dark" | "light" } }"""
    --> }

-}
at : List String -> Decoder value -> Decoder value
at location (Decoder innerDecoder innerType) =
    Decoder
        (Decode.at location innerDecoder)
        (location
            |> List.foldr
                (\fieldName typeSoFar ->
                    TypeObject [ ( Required, fieldName, typeSoFar ) ]
                )
                innerType
        )


{-|

    import Json.Decode


    string
        |> runExample """ "Hello!" """
    --> { decoded = Ok "Hello!"
    --> , tsType = "string"
    --> }

-}
string : Decoder String
string =
    Decoder Decode.string String


{-|

    import Json.Decode


    int
        |> runExample "1000"
    --> { decoded = Ok 1000
    --> , tsType = "number"
    --> }

Floating point values will cause a decoding error.

    int
        |> runExample "1.23"
    --> { decoded = Err "Problem with the given value:\n\n1.23\n\nExpecting an INT"
    --> , tsType = "number"
    --> }

-}
int : Decoder Int
int =
    Decoder Decode.int Integer


{-|

    import Json.Decode


    float
        |> runExample "1.23"
    --> { decoded = Ok 1.23
    --> , tsType = "number"
    --> }

-}
float : Decoder Float
float =
    Decoder Decode.float Number


{-|

    import Json.Decode


    bool
        |> runExample "true"
    --> { decoded = Ok True
    --> , tsType = "boolean"
    --> }

-}
bool : Decoder Bool
bool =
    Decoder Decode.bool Boolean


{-|

    import Json.Decode


    list int
        |> runExample "[1,2,3]"
    --> { decoded = Ok [ 1, 2, 3 ]
    --> , tsType = "number[]"
    --> }

-}
list : Decoder value -> Decoder (List value)
list (Decoder innerDecoder innerType) =
    Decoder (Decode.list innerDecoder) (List innerType)


{-|

    import Json.Decode


    index 1 int
        |> runExample "[0,100,200]"
    --> { decoded = Ok 100
    --> , tsType = "[JsonValue,number,...JsonValue[]]"
    --> }

    map2 Tuple.pair
        ( index 1 int )
        ( index 3 string )
        |> runExample """[0,100,"a","b"]"""
    --> { decoded = Ok ( 100, "b" )
    --> , tsType = "[JsonValue,number,JsonValue,string,...JsonValue[]]"
    --> }

-}
index : Int -> Decoder value -> Decoder value
index n (Decoder innerDecoder innerType) =
    Decoder (Decode.index n innerDecoder) (ArrayIndex ( n, innerType ) [])


{-|

    import Json.Decode


    tuple string int
        |> runExample """["abc", 123]"""
    --> { decoded = Ok ( "abc", 123 )
    --> , tsType = "[ string, number ]"
    --> }

-}
tuple : Decoder value1 -> Decoder value2 -> Decoder ( value1, value2 )
tuple decoder1 decoder2 =
    startTuple Tuple.pair
        |> element decoder1
        |> element decoder2
        |> buildTuple


{-|

    import Json.Decode


    triple string int bool
        |> runExample """["abc", 123, true]"""
    --> { decoded = Ok ( "abc", 123, True )
    --> , tsType = "[ string, number, boolean ]"
    --> }

-}
triple :
    Decoder value1
    -> Decoder value2
    -> Decoder value3
    -> Decoder ( value1, value2, value3 )
triple decoder1 decoder2 decoder3 =
    startTuple (\a b c -> ( a, b, c ))
        |> element decoder1
        |> element decoder2
        |> element decoder3
        |> buildTuple


{-| -}
startTuple : a -> TupleBuilder a
startTuple constructor =
    TupleBuilder (Decode.succeed constructor) []


{-| -}
buildTuple : TupleBuilder a -> Decoder a
buildTuple (TupleBuilder pipelineDecoder pipelineType) =
    Decoder pipelineDecoder (Tuple pipelineType Nothing)


{-| -}
type TupleBuilder value
    = TupleBuilder (Decode.Decoder value) (List TsType)


{-|

    import Json.Decode


    startTuple (\a b c d -> { a = a, b = b, c = c, d = d })
        |> element string
        |> element int
        |> element bool
        |> element string
        |> buildTuple
        |> runExample """["abc", 123, true, "xyz"]"""
    --> { decoded = Ok { a = "abc", b = 123, c = True, d = "xyz" }
    --> , tsType = "[ string, number, boolean, string ]"
    --> }

-}
element :
    Decoder a
    -> TupleBuilder (a -> b)
    -> TupleBuilder b
element (Decoder innerDecoder1 innerType1) (TupleBuilder pipelineDecoder pipelineType) =
    TupleBuilder
        (pipelineDecoder |> Decode.map2 (|>) (Decode.index (List.length pipelineType) innerDecoder1))
        (pipelineType ++ [ innerType1 ])


{-|

    import Json.Decode
    import Json.Encode


    oneOrMore (::) int
        |> runExample "[12345]"
    --> { decoded = Ok [ 12345 ]
    --> , tsType = """[ number, ...(number)[] ]"""
    --> }

    type TestResult
        = Pass
        | Fail String

    testCaseDecoder : Decoder TestResult
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
    --> , tsType = """[ { tag : "pass" } | { message : string; tag : "fail" }, ...({ tag : "pass" } | { message : string; tag : "fail" })[] ]"""
    --> }

-}
oneOrMore : (a -> List a -> value) -> Decoder a -> Decoder value
oneOrMore constructor (Decoder innerDecoder innerType) =
    Decoder (Decode.oneOrMore constructor innerDecoder) (Tuple [ innerType ] (Just innerType))


{-| Exactly the same as the [`list`](#list) `Decoder` except that it wraps the decoded `List` into an Elm `Array`.
-}
array : Decoder value -> Decoder (Array value)
array (Decoder innerDecoder innerType) =
    Decoder (Decode.array innerDecoder) (List innerType)


{-|

    import Json.Decode
    import Json.Encode
    import Dict exposing (Dict)


    dict int
        |> runExample """{"alice":42,"bob":99}"""
    --> { decoded = Ok (Dict.fromList [ ( "alice", 42 ), ( "bob", 99 ) ])
    --> , tsType = "{ [key: string]: number }"
    --> }

-}
dict : Decoder value -> Decoder (Dict String value)
dict (Decoder innerDecoder innerType) =
    Decoder (Decode.dict innerDecoder) (ObjectWithUniformValues innerType)


{-|

    import Json.Decode


    keyValuePairs int
        |> runExample """{ "alice": 42, "bob": 99 }"""
    --> { decoded = Ok [ ( "alice", 42 ), ( "bob", 99 ) ]
    --> , tsType = "{ [key: string]: number }"
    --> }

-}
keyValuePairs : Decoder value -> Decoder (List ( String, value ))
keyValuePairs (Decoder innerDecoder innerType) =
    Decoder (Decode.keyValuePairs innerDecoder) (ObjectWithUniformValues innerType)


{-| Get a regular JSON Decoder that you can run using the `elm/json` API.
-}
decoder : Decoder value -> Decode.Decoder value
decoder (Decoder decoder_ _) =
    decoder_


{-| -}
tsType : Decoder value -> TsType
tsType (Decoder _ tsType_) =
    tsType_


{-| -}
runExample :
    String
    -> Decoder value
    -> { decoded : Result String value, tsType : String }
runExample inputJson interopDecoder =
    { tsType = interopDecoder |> tsType |> TsType.toString
    , decoded = Decode.decodeString (decoder interopDecoder) inputJson |> Result.mapError Decode.errorToString
    }
