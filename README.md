# `elm-ts-interop` [![Build Status](https://github.com/dillonkearns/elm-ts-interop/workflows/CI/badge.svg)](https://github.com/dillonkearns/elm-ts-interop/actions?query=branch%3Amain)

Build up Encoders/Decoders with well-defined TypeScript types! This is different than the approach of [`elm-typescript-interop`](https://github.com/dillonkearns/elm-typescript-interop).

You can read more about the redesign in this article: [Types Without Borders Isn't Enough](https://functional.christmas/2020/11).

## Core concepts

### Provide type information about your Elm app

TypeScript declaration files (with extension `.d.ts`) allow you to supplement plain JS (like compiled Elm code) with TypeScript type information. `elm-ts-interop` creates declaration files so that you get autocompletion and type errors as you wire up your ports, so you can be confident that you're sending/expecting the right data types.

### Figure out the types for your ports (to and from Elm)

How does `elm-ts-interop` figure out the types of your ports? There's no magic involved at all. You define a special type of Encoder/Decoder using the `TsInterop.Encoder` and `TsInterop.Decoder` modules.

These `Decoder`s and `Encoder`s that you write are the source of truth. It doesn't rely on implicitly generated Decoders or Encoders
(Evan generally recommends against implicitly defined serialization because of limitations to that approach that he describes in his [vision for data interchange](https://gist.github.com/evancz/1c5f2cf34939336ecb79b97bb89d9da6#gistcomment-2606737)). Instead, you define code with an API that is very similar to `elm/json`. But this API knows the type information of the Encoder or Decoder you've built.

For example, if you use `TsDecode.string`, it knows that will expect to receive a `string` from TypeScript. `TsDecode.list TsDecode.string`
will expect to receive `string[]` from TypeScript. You can even build up more expressive TypeScript types like Unions:

```elm
    TsDecode.oneOf [ TsDecode.string TsDecode.map, TsDecode.bool |> TsDecode.map boolToString ]
```

This will expect to receive a `string | boolean` from TypeScript! From this simple idea, you can build very sophisticated
typed interop, even sending/receiving Elm Custom Types, and sending/receiving TypeScript discriminated unions.

## Example

```elm
import Json.Encode as JE
import TsInterop.Encode as Encode exposing (Encoder)
import TsInterop.Decode as Decode exposing (Decoder)

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

userEncoder |> Encode.typeDef
--> """{"kind":"guest"} | { kind : "regular"; name : string }"""

userDecoder |> Decode.tsTypeToString
--> """{ name : string; kind : "regular" } | {"kind":"guest"}"""
```

## Do I need to pay to use elm-ts-interop?

You don't need to pay, and the core features of `elm-ts-interop` will always be free. If you're getting value from `elm-ts-interop` and you'd like to get some additional code generation features that make it nicer to work with, then you can upgrade to the pro version at elm-ts-interop.com. Thank you to pro users for supporting my work!

## Wasn't this called elm-typescript-interop before?

Yes, you can think of `elm-ts-interop` as version 2.0 of `elm-typescript-interop` (which is now deprecated). The old approach scanned your Elm project's source code to find all ports and their types. This worked, but it had limitations. Some of the benefits of the new approach with `elm-ts-interop` include:

- No performance bottlenecks for large apps - performance is the same no matter how big your source code is because it doesn't scan your whole app
- You can now send/receive Custom Types through ports!
- The types you can send/receive through ports are now much more expressive. The types you can annotate an Elm port with to automatically serialize are limited to Bool, Int, Float, String, Maybe, List, Array, tuples, records, and Json.Decode.Value. With `elm-ts-interop`, you can serialize/de-serialize anything that you could express through a TypeScript annotation of a JSON value. That includes:
  - array
  - unions (including discriminated unions, which can express the same possibilities as an Elm Custom type!)
  - Literal types, like `type role = 'admin' | 'regular' | 'guest'`

## Can I use elm-ts-interop with plain JavaScript?

You absolutely can! `elm-ts-interop` works great with plain `.js` files.

Here are some docs on using TypeScript through JSDoc comments in JS: <https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html>.
