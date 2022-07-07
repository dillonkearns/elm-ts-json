module CodecBaseTests exposing (suite)

import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Test exposing (Test, describe, fuzz, test)
import TsJson.Codec as TsCodec exposing (Codec)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import TsJson.Type
import TsType


suite : Test
suite =
    describe "Testing roundtrips"
        [ describe "Basic" basicTests
        , describe "Containers" containersTests
        , describe "Object" objectTests
        , describe "Custom" customTests
        , describe "bimap" bimapTests
        , describe "maybe" maybeTests
        , describe "succeed"
            [ test "roundtrips"
                (\_ ->
                    TsCodec.succeed 632
                        |> (\d -> Decode.decodeString (TsCodec.decoder d |> TsDecode.decoder) "{}")
                        |> Expect.equal (Ok 632)
                )
            ]
        , describe "recursive" recursiveTests
        , describe "map,andThen" mapAndThenTests
        ]


roundtrips : Fuzzer a -> Codec a -> Test
roundtrips fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            let
                encoded =
                    value
                        |> TsEncode.encoder (TsCodec.encoder codec)
            in
            encoded
                |> (TsCodec.decoder codec
                        |> TsDecode.decoder
                        |> Decode.decodeValue
                   )
                |> Result.mapError Decode.errorToString
                |> Expect.all
                    [ Expect.equal (Ok value)
                    , \_ ->
                        Expect.equal
                            (codec |> TsCodec.encoder |> TsEncode.tsType |> TsJson.Type.toTypeScript)
                            (codec |> TsCodec.decoder |> TsDecode.tsType |> TsJson.Type.toTypeScript)
                    ]


roundtripsWithDifferentAnnotations : Fuzzer a -> Codec a -> Test
roundtripsWithDifferentAnnotations fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            value
                |> TsEncode.encoder (TsCodec.encoder codec)
                |> (TsCodec.decoder codec
                        |> TsDecode.decoder
                        |> Decode.decodeValue
                   )
                |> Expect.equal (Ok value)


roundtripsWithin : Fuzzer Float -> Codec Float -> Test
roundtripsWithin fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            value
                |> TsEncode.encoder (TsCodec.encoder codec)
                |> (TsCodec.decoder codec
                        |> TsDecode.decoder
                        |> Decode.decodeValue
                   )
                |> Result.withDefault -999.1234567
                |> Expect.within (Expect.Relative 0.000001) value


basicTests : List Test
basicTests =
    [ describe "Codec.string"
        [ roundtrips Fuzz.string TsCodec.string
        ]
    , describe "Codec.int"
        [ roundtrips Fuzz.int TsCodec.int
        ]
    , describe "Codec.float"
        [ roundtrips Fuzz.float TsCodec.float
        ]
    , describe "Codec.bool"
        [ roundtrips Fuzz.bool TsCodec.bool
        ]
    ]


containersTests : List Test
containersTests =
    [ describe "Codec.array"
        [ roundtrips (Fuzz.array Fuzz.int) (TsCodec.array TsCodec.int)
        ]
    , describe "Codec.list"
        [ roundtrips (Fuzz.list Fuzz.int) (TsCodec.list TsCodec.int)
        ]
    , describe "Codec.dict"
        [ roundtrips
            (Fuzz.map2 Tuple.pair Fuzz.string Fuzz.int
                |> Fuzz.list
                |> Fuzz.map Dict.fromList
            )
            (TsCodec.dict TsCodec.int)
        ]
    , describe "Codec.set"
        [ roundtrips
            (Fuzz.list Fuzz.int |> Fuzz.map Set.fromList)
            (TsCodec.set TsCodec.int)
        ]
    , describe "Codec.tuple"
        [ roundtrips
            (Fuzz.tuple ( Fuzz.int, Fuzz.int ))
            (TsCodec.tuple TsCodec.int TsCodec.int)
        ]
    ]


objectTests : List Test
objectTests =
    [ describe "with 0 fields"
        [ roundtripsWithDifferentAnnotations (Fuzz.constant {})
            (TsCodec.object {}
                |> TsCodec.buildObject
            )
        ]
    , describe "with 1 field"
        [ roundtrips (Fuzz.map (\i -> { fname = i }) Fuzz.int)
            (TsCodec.object (\i -> { fname = i })
                |> TsCodec.field "fname" .fname TsCodec.int
                |> TsCodec.buildObject
            )
        ]
    , describe "with 2 fields"
        [ roundtrips
            (Fuzz.map2
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                Fuzz.int
                Fuzz.int
            )
            (TsCodec.object
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> TsCodec.field "a" .a TsCodec.int
                |> TsCodec.field "b" .b TsCodec.int
                |> TsCodec.buildObject
            )
        ]
    , describe "nullableField vs maybeField" <|
        let
            nullableCodec =
                TsCodec.object
                    (\f -> { f = f })
                    |> TsCodec.nullableField "f" .f TsCodec.int
                    |> TsCodec.buildObject

            maybeCodec =
                TsCodec.object
                    (\f -> { f = f })
                    |> TsCodec.maybeField "f" .f TsCodec.int
                    |> TsCodec.buildObject
        in
        [ test "a nullableField is required" <|
            \_ ->
                "{}"
                    |> decodeString nullableCodec
                    |> (\r ->
                            case r of
                                Ok _ ->
                                    Expect.fail "Should have failed"

                                Err _ ->
                                    Expect.pass
                       )
        , test "a nullableField produces a field with a null value on encoding Nothing" <|
            \_ ->
                { f = Nothing }
                    |> encodeToString nullableCodec
                    |> Expect.equal "{\"f\":null}"
        , test "a maybeField is optional" <|
            \_ ->
                "{}"
                    |> decodeString maybeCodec
                    |> Expect.equal (Ok { f = Nothing })
        , test "a maybeField doesn't produce a field on encoding Nothing" <|
            \_ ->
                { f = Nothing }
                    |> encodeToString maybeCodec
                    |> Expect.equal "{}"
        ]
    ]


encodeToString : Codec input -> (input -> String)
encodeToString codec =
    (codec
        |> TsCodec.encoder
        |> TsEncode.encoder
    )
        >> Encode.encode 0


decodeString : Codec a -> String -> Result Decode.Error a
decodeString codec =
    TsCodec.decoder codec
        |> TsDecode.decoder
        |> Decode.decodeString


type Newtype a
    = Newtype a


customTests : List Test
customTests =
    [ describe "with 1 ctor, 0 args"
        [ roundtrips (Fuzz.constant ())
            (TsCodec.custom Nothing
                (\f v ->
                    case v of
                        () ->
                            f
                )
                |> TsCodec.variant0 "()" ()
                |> TsCodec.buildCustom
            )
        ]
    , describe "with 1 ctor, 1 arg"
        [ roundtrips (Fuzz.map Newtype Fuzz.int)
            (TsCodec.custom Nothing
                (\f v ->
                    case v of
                        Newtype a ->
                            f a
                )
                |> TsCodec.positionalVariant1 "Newtype" Newtype TsCodec.int
                |> TsCodec.buildCustom
            )
        ]
    , describe "with 2 ctors, 0,1 args" <|
        let
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec =
                TsCodec.custom Nothing match
                    |> TsCodec.variant0 "Nothing" Nothing
                    |> TsCodec.positionalVariant1 "Just" Just TsCodec.int
                    |> TsCodec.buildCustom

            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just Fuzz.int )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips fuzz codec ]
                )
    , describe "with 2 ctors, 0,2 args" <|
        let
            match : TsEncode.UnionEncodeValue -> (Int -> Int -> TsEncode.UnionEncodeValue) -> Maybe ( Int, Int ) -> TsEncode.UnionEncodeValue
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just ( v1, v2 ) ->
                        fjust v1 v2

            codec : Codec (Maybe ( Int, Int ))
            codec =
                TsCodec.custom Nothing match
                    |> TsCodec.variant0 "Nothing" Nothing
                    |> TsCodec.positionalVariant2 "Just" (\first second -> Just ( first, second )) TsCodec.int TsCodec.int
                    |> TsCodec.buildCustom
        in
        [ ( "1st ctor", Fuzz.constant Nothing )
        , ( "2nd ctor", Fuzz.map2 (\a b -> Just ( a, b )) Fuzz.int Fuzz.int )
        ]
            |> roundtripsTest "codec type"
                codec
                """{ args : [ number, number ]; tag : "Just" } | { tag : "Nothing" }"""
    , describe "with 3 ctors, 0,3 args" <|
        let
            codec : Codec MyCustomType
            codec =
                TsCodec.custom Nothing
                    (\fSingle fTriple value ->
                        case value of
                            Single v1 ->
                                fSingle v1

                            Triple v1 v2 v3 ->
                                fTriple v1 v2 v3
                    )
                    |> TsCodec.positionalVariant1 "Single" Single TsCodec.int
                    |> TsCodec.positionalVariant3 "Triple" (\v1 v2 v3 -> Triple v1 v2 v3) TsCodec.int TsCodec.int TsCodec.int
                    |> TsCodec.buildCustom
        in
        [ ( "1st ctor", Fuzz.map Single Fuzz.int )
        , ( "2nd ctor", Fuzz.map3 Triple Fuzz.int Fuzz.int Fuzz.int )
        ]
            |> roundtripsTest "codec type"
                codec
                """{ args : [ number, number, number ]; tag : "Triple" } | { args : [ number ]; tag : "Single" }"""
    , describe "proper quotes for object keys" <|
        let
            codec : Codec (Newtype Int)
            codec =
                TsCodec.custom (Just "my-discriminator")
                    (\f v ->
                        case v of
                            Newtype a ->
                                f a
                    )
                    |> TsCodec.namedVariant1 "new-type" Newtype ( "my-int", TsCodec.int )
                    |> TsCodec.buildCustom
        in
        [ ( "int", Fuzz.map Newtype Fuzz.int ) ]
            |> roundtripsTest "codec type"
                codec
                """{ "my-discriminator" : "new-type"; "my-int" : number }"""
    , describe "stringUnion" <|
        let
            codec : Codec DarkMode
            codec =
                TsCodec.stringUnion [ ( "dark", Dark ), ( "light", Light ) ]
        in
        [ ( "dark", Fuzz.constant Dark )
        , ( "light", Fuzz.constant Light )
        ]
            |> roundtripsTest "dark mode codec"
                codec
                "\"dark\" | \"light\""
    , describe "literal" <|
        let
            codec : Codec String
            codec =
                TsCodec.literal "Hello" (Encode.list Encode.string [ "Hello" ])
        in
        [ ( "Ok test", Fuzz.constant "Hello" )
        ]
            |> roundtripsTest "dark mode codec"
                codec
                """["Hello"]"""
    , describe "string literal" <|
        let
            codec : Codec ()
            codec =
                TsCodec.stringLiteral () "Hi"
        in
        [ ( "Ok test", Fuzz.constant () )
        ]
            |> roundtripsTest "dark mode codec"
                codec
                "\"Hi\""
    ]


type DarkMode
    = Dark
    | Light


type MyCustomType
    = Single Int
    | Triple Int Int Int


roundtripsTest :
    String
    -> Codec value
    -> String
    -> List ( String, Fuzzer value )
    -> List Test
roundtripsTest testName codec expectedTsType fuzzers =
    (test testName <|
        \() ->
            codec
                |> TsCodec.tsType
                |> TsType.toString
                |> Expect.equal expectedTsType
    )
        :: (fuzzers
                |> List.map
                    (\( name, fuzz ) ->
                        describe name
                            [ roundtrips fuzz codec ]
                    )
           )


bimapTests : List Test
bimapTests =
    [ roundtripsWithin Fuzz.float <|
        TsCodec.map
            (\x -> x * 2)
            (\x -> x / 2)
            TsCodec.float
    ]


maybeTests : List Test
maybeTests =
    [ describe "single"
        [ roundtripsWithDifferentAnnotations
            (Fuzz.oneOf
                [ Fuzz.constant Nothing
                , Fuzz.map Just Fuzz.int
                ]
            )
          <|
            TsCodec.maybe TsCodec.int
        ]

    {-
       This is a known limitation: using null as Nothing and identity as Just means that nesting two maybes squashes Just Nothing with Nothing
       , describe "double"
          [ roundtrips
              (Fuzz.oneOf
                  [ Fuzz.constant Nothing
                  , Fuzz.constant <| Just Nothing
                  , Fuzz.map (Just << Just) Fuzz.int
                  ]
              )
            <|
              Codec.maybe <|
                  Codec.maybe Codec.int
          ]
    -}
    ]


recursiveTests : List Test
recursiveTests =
    [ ( "list", Fuzz.list Fuzz.int ) ]
        |> roundtripsTest "recursive list"
            (TsCodec.recursive
                (\c ->
                    TsCodec.custom Nothing
                        (\fempty fcons value ->
                            case value of
                                [] ->
                                    fempty

                                x :: xs ->
                                    fcons x xs
                        )
                        |> TsCodec.variant0 "[]" []
                        |> TsCodec.positionalVariant2 "(::)" (::) TsCodec.int c
                        |> TsCodec.buildCustom
                )
            )
            """{ args : [ number, JsonValue ]; tag : "(::)" } | { tag : "[]" }"""


mapAndThenTests : List Test
mapAndThenTests =
    [ describe "Codec.map"
        [ roundtrips (Fuzz.intRange -10000 10000) <|
            TsCodec.map (\x -> x - 1) (\x -> x + 1) TsCodec.int
        ]
    ]
