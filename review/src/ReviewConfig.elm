module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Modules.rule
    , NoUnused.Exports.rule
    , NoUnused.Dependencies.rule
    , NoUnused.CustomTypeConstructorArgs.rule
        |> ignoreForTests

    --, NoUnused.Variables.rule
    -- , NoUnused.CustomTypeConstructors.rule []
    --, NoUnused.Parameters.rule
    --, NoUnused.Patterns.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> ignoreForTests
    , NoExposingEverything.rule

    --, NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule

    , NoInconsistentAliases.config
       [ ( "TsJson.Decode", "TsDecode" )
       , ( "TsJson.Encode", "TsEncode" )
       , ( "TsJson.Codec", "TsCodec" )
       , ( "Json.Decode", "Decode" )
       , ( "Json.Encode", "Encode" )
       ]
       |> NoInconsistentAliases.noMissingAliases
       |> NoInconsistentAliases.rule
    -- , Docs.NoMissing.rule
    --     { document = onlyExposed
    --     , from = exposedModules
    --     }
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    ]
        |> List.map
            (\rule ->
                rule
                    |> Review.Rule.ignoreErrorsForDirectories [ "src/List" ]
            )


ignoreForTests : Rule -> Rule
ignoreForTests rule =
    rule
        |> Review.Rule.ignoreErrorsForDirectories [ "tests" ]
