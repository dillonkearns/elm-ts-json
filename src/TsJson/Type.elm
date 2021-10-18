module TsJson.Type exposing
    ( Type
    , toTypeScript, toJsonSchema
    )

{-| Usually you don't need to use this module directly, but instead use a tool that makes use of this module (like [`elm-ts-interop`](https://github.com/dillonkearns/elm-ts-interop)).

@docs Type
@docs toTypeScript, toJsonSchema

-}

import Internal.TsJsonType
import Json.Encode as Encode
import TsType


{-| Represents a JSON value with TypeScript information.
-}
type alias Type =
    Internal.TsJsonType.TsType


{-| You can express the same type information about a JSON values with [JSON Schema](https://json-schema.org/) format.
One example where this can be useful is if tooling wants to perform a runtime check of a JSON value before passing it to a TsJson.Decoder to ensure
that it is a valid input.
-}
toJsonSchema : Type -> Encode.Value
toJsonSchema =
    TsType.toJsonSchema


{-| Tooling can use this function to convert the JSON type information into a string describing a TypeScript type.

Note that instead of the TypeScript type `unknown`, this will use a type called `JsonValue` to be more precise.
This isn't a built-in TypeScript type, but since these values can only be JSON, it represents the constraints more accurately.

You can find the type definition of `JsonValue` here if you are building your own custom tooling and need to handle this type:
<https://github.com/sindresorhus/type-fest/blob/4c9835b3c42d7a9f1d10bae19c334ce1d8e8c8a4/source/basic.d.ts#L22-L37>.

-}
toTypeScript : Type -> String
toTypeScript =
    TsType.toString
