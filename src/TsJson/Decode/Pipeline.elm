module TsJson.Decode.Pipeline exposing (required, requiredAt, optional, optionalAt, hardcoded, custom)

{-| Use the `(|>)` operator to build JSON decoders.

This is a typed fork of [NoRedInk/elm-json-decode-pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest). Thanks to NoRedInk for the original API!


## Decoding fields

@docs required, requiredAt, optional, optionalAt, hardcoded, custom

-}

import Internal.TsJsonType as TsType
import Internal.TypeReducer as TypeReducer
import Json.Decode
import Json.Encode
import TsJson.Decode as Decode exposing (Decoder)
import TsJson.Internal.Decode


{-| Decode a required field.

    import TsJson.Decode Decode exposing (Decoder, int, string)
    import TsJson.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "name" string
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com", "name": "Sam"}
        """

    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

-}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
    custom (Decode.field key valDecoder) decoder


{-| Decode a required nested field.
-}
requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path valDecoder decoder =
    custom (Decode.at path valDecoder) decoder


{-| Decode a field that may be missing or have a null value. If the field is
missing, then it decodes as the `fallback` value. If the field is present,
then `valDecoder` is used to decode its value. If `valDecoder` fails on a
`null` value, then the `fallback` is used as if the field were missing
entirely.

    import TsJson.Decode Decode exposing (Decoder, int, null, oneOf, string)
    import TsJson.Decode.Pipeline exposing (optional, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> optional "name" string "blah"
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com" }
        """

    -- Ok { id = 123, name = "blah", email = "sam@example.com" }

Because `valDecoder` is given an opportunity to decode `null` values before
resorting to the `fallback`, you can distinguish between missing and `null`
values if you need to:

    userDecoder2 =
        Decode.succeed User
            |> required "id" int
            |> optional "name" (oneOf [ string, null "NULL" ]) "MISSING"
            |> required "email" string

-}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional key valDecoder fallback decoder =
    custom (optionalDecoder [ key ] (Decode.field key Decode.value) valDecoder fallback) decoder


{-| Decode an optional nested field.
-}
optionalAt : List String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optionalAt path valDecoder fallback decoder =
    custom (optionalDecoder path (Decode.at path Decode.value) valDecoder fallback) decoder


optionalDecoder : List String -> Decoder Json.Decode.Value -> Decoder a -> a -> Decoder a
optionalDecoder path pathDecoder valDecoder fallback =
    let
        nullOr : Decoder a -> Decoder a
        nullOr decoder =
            Decode.oneOf [ decoder, Decode.null fallback ]

        handleResult : Json.Decode.Value -> Json.Decode.Decoder a
        handleResult input =
            case Json.Decode.decodeValue (Decode.decoder pathDecoder) input of
                Ok rawValue ->
                    -- The field was present, so now let's try to decode that value.
                    -- (If it was present but fails to decode, this should and will fail!)
                    case rawValue |> Json.Decode.decodeValue (Decode.decoder (nullOr valDecoder)) of
                        Ok finalResult ->
                            Json.Decode.succeed finalResult

                        Err finalErr ->
                            -- TODO is there some way to preserve the structure
                            -- of the original error instead of using toString here?
                            Json.Decode.fail (Json.Decode.errorToString finalErr)

                Err _ ->
                    -- The field was not present, so use the fallback.
                    Json.Decode.succeed fallback
    in
    TsJson.Internal.Decode.Decoder (Json.Decode.value |> Json.Decode.andThen handleResult)
        (optionalAtType path (Decode.tsType valDecoder))


optionalAtType : List String -> TsType.TsType -> TsType.TsType
optionalAtType path innerType =
    case path of
        [] ->
            innerType

        key :: remainingPath ->
            TsType.TypeObject
                [ ( TsType.Optional, key, optionalAtType remainingPath innerType ) ]


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcoded` does not look at the JSON at all.

    import TsJson.Decode Decode exposing (Decoder, int, string)
    import TsJson.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , email : String
        , followers : Int
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "email" string
            |> hardcoded 0

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com"}
        """

    -- Ok { id = 123, email = "sam@example.com", followers = 0 }

-}
hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded =
    Decode.succeed >> custom


{-| Run the given decoder and feed its result into the pipeline at this point.

Consider this example.

    import TsJson.Decode Decode exposing (Decoder, at, int, string)
    import TsJson.Decode.Pipeline exposing (custom, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> custom (at [ "profile", "name" ] string)
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {
            "id": 123,
            "email": "sam@example.com",
            "profile": {"name": "Sam"}
          }
        """

    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

-}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom =
    Decode.map2 (|>)
