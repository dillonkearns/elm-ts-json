# `elm-ts-json` [![Build Status](https://github.com/dillonkearns/elm-ts-json/workflows/CI/badge.svg)](https://github.com/dillonkearns/elm-ts-json/actions?query=branch%3Amain)

Build up Encoders/Decoders with well-defined TypeScript types! The API in this package is the foundation of the [`elm-ts-interop`](https://github.com/dillonkearns/elm-ts-interop) CLI tool, but can be used for other purposes as well.

## Core concepts

### Figure out the types for your ports (to and from Elm)

How does `elm-ts-interop` figure out the types of your ports? There's no magic involved at all. You define a special type of Encoder/Decoder using the `TsJson.Encoder` and `TsJson.Decoder` modules.

These `Decoder`s and `Encoder`s that you write are the source of truth. It doesn't rely on implicitly generated Decoders or Encoders
(Evan generally recommends against implicitly defined serialization because of limitations to that approach that he describes in his [vision for data interchange](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6#gistcomment-2606737)). Instead, you define code with an API that is very similar to `elm/json`. But this API knows the type information of the Encoder or Decoder you've built.

For example, if you use `TsDecode.string`, it knows that will expect to receive a `string` from TypeScript. `TsDecode.list TsDecode.string`
will expect to receive `string[]` from TypeScript. You can even build up more expressive TypeScript types like Unions:

```elm
import TsJson.Decode as TsDecode
import TsJson.Type

TsDecode.oneOf
    [ TsDecode.string
    , TsDecode.int |> TsDecode.map String.fromInt
    ]
    |> TsDecode.tsType
    |> TsJson.Type.toTypeScript
    --> "string | number"
```

So we've written a Decoder very similar to how we would with `elm/json`, but this `Decoder` has one key difference.
We can turn the `Decoder` itself into a TypeScript type (`string | number`). And without even passing a value through the `Decoder`
 From this simple idea, you can build very sophisticated typed interop, even sending/receiving Elm Custom Types,
and sending/receiving TypeScript discriminated unions.

## Example

```elm
import Json.Encode as JE
import TsJson.Encode as Encode exposing (Encoder)
import TsJson.Decode as Decode exposing (Decoder)

type User
    = Regular { name : String }
    | Guest

userEncoder : Encoder User
userEncoder =
    Encode.union
        (\vRegular vGuest value ->
            case value of
                Regular data ->
                    vRegular data
                Guest ->
                    vGuest
        )
        |> Encode.variant
            (Encode.object
                [ Encode.required "kind" identity (Encode.literal <| JE.string "regular")
                , Encode.required "name" .name Encode.string
                ]
            )
        |> Encode.variantLiteral (JE.object [ ( "kind", JE.string "guest" ) ])
        |> Encode.buildUnion


userDecoder : Decoder User
userDecoder =
    Decode.oneOf
        [ Decode.succeed (\() name -> Regular { name = name })
            |> Decode.andMap (Decode.field "kind" (Decode.literal () (JE.string "regular")))
            |> Decode.andMap (Decode.field "name" Decode.string)
        , Decode.literal Guest (JE.object [ ( "kind", JE.string "guest" ) ])
        ]

type alias Name =
    { first : String, last : String }


nameEncoder : Encoder Name
nameEncoder =
    Encode.object
        [ Encode.required "first" .first Encode.string
        , Encode.required "last" .last Encode.string
        ]


nameDecoder : Decoder Name
nameDecoder =
    Decode.succeed Name
        |> Decode.andMap (Decode.field "first" Decode.string)
        |> Decode.andMap (Decode.field "last" Decode.string)

Guest
    |> Encode.runExample userEncoder
--> { output = """{"kind":"guest"}"""
--> , tsType = """{"kind":"guest"} | { kind : "regular"; name : string }"""
--> }

userDecoder |> Decode.runExample """{"kind":"guest"}"""
--> { decoded = Ok Guest
--> , tsType = """{ kind : "regular"; name : string } | {"kind":"guest"}"""
--> }
```
