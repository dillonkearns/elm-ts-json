# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-ts-json.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/)

All notable changes to
[`dillonkearns/elm-ts-json` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.1.0] - 2021-03-10

### Fixed

- Add `TsJson.Encodde.variantTagged` function.
- Remove duplicate object properties when a decoder decodes a field multiple times. TypeScript was giving the `Duplicate identifier 'foo'.(2300)` error for this output. The toTypeScript function no longer will result in repeated properties.
