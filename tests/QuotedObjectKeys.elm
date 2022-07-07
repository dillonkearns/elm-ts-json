module QuotedObjectKeys exposing (suite)

import Expect
import Internal.TypeToString exposing (quoteObjectKey)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Testing quoted object keys"
        [ describe "object keys that should be quoted" <|
            List.map
                (\key ->
                    let
                        quotedKey =
                            doubleQuoteforTest key

                        testName =
                            "key " ++ key ++ " should be quoted " ++ quotedKey
                    in
                    quoteObjectKey key
                        |> Expect.equal quotedKey
                        |> always
                        |> test testName
                )
                objectKeysShouldBeQuoted
        , describe "object keys that should not be quoted" <|
            List.map
                (\key ->
                    let
                        testName =
                            "key " ++ key ++ " should not be quoted"
                    in
                    quoteObjectKey key
                        |> Expect.equal key
                        |> always
                        |> test testName
                )
                objectKeysNotQuoted
        ]


doubleQuoteforTest : String -> String
doubleQuoteforTest string =
    String.concat [ "\"", string, "\"" ]


objectKeysShouldBeQuoted : List String
objectKeysShouldBeQuoted =
    [ " a"
    , " "
    , "a b"
    , "a "
    , "0"
    , "0a"
    , "-"
    , "-a"
    , "a-"
    , "☺"
    , "a☺b"
    , "a-b"
    , "A#B"
    , "A'B"
    , "a+b"
    , "a*b"
    , "a~b"
    , "a`b"
    ]


objectKeysNotQuoted : List String
objectKeysNotQuoted =
    [ "a"
    , "_"
    , "_a"
    , "a0"
    , "a_"
    , "A"
    , "_A"
    , "A0"
    , "A_"
    , "b_1B"
    ]
