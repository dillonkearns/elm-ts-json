# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-ts-json.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest/)

All notable changes to
[`dillonkearns/elm-ts-json` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-ts-json/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.1.1] - 2022-08-24

### Fixed

- Object keys are correctly quoted in cases where unquoted keys cause invalid syntax. See [#12](https://github.com/dillonkearns/elm-ts-json/pull/12). Thank you [@pabra](https://github.com/pabra) for the fix!

## [2.1.0] - 2021-10-18

### Added

- `TsJson.Decode.discriminatedUnion` helper makes it easier to do simple unions
  that use the discriminated union convention.
- `TsJson.Decode.stringUnion` for simple enumeration-style string unions.
- Added `stringLiteral` for `Codec` and `Decode` modules

## [2.0.0] - 2021-10-11

### Added

- New Codec API

### Changed

- Decoder and Encoder types point to new internal type to avoid future breaking
  changes. This won't actually break code, but Elm will mark it as a breaking
  change because the underlying type is different.

## [1.1.0] - 2021-03-10

### Fixed

- Add `TsJson.Encodde.variantTagged` function.
- Remove duplicate object properties when a decoder decodes a field multiple times. TypeScript was giving the `Duplicate identifier 'foo'.(2300)` error for this output. The toTypeScript function no longer will result in repeated properties.
