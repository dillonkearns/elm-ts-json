# `elm-ts-interop` [![Build Status](https://github.com/dillonkearns/elm-ts-interop/workflows/CI/badge.svg)](https://github.com/dillonkearns/elm-ts-interop/actions?query=branch%3Amain)

Build up Encoders/Decoders with well-defined TypeScript types! This is different than the approach of [`elm-typescript-interop`](https://github.com/dillonkearns/elm-typescript-interop).

You can read more about the redesign in this article: [Types Without Borders Isn't Enough](https://functional.christmas/2020/11).

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
