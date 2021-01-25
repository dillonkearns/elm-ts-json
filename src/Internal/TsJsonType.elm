module Internal.TsJsonType exposing (PropertyOptionality(..), TsType(..))

import Json.Encode as Encode


type PropertyOptionality
    = Optional
    | Required


type TsType
    = String
    | Integer
    | Number
    | Boolean
    | List TsType
    | ArrayIndex ( Int, TsType ) (List ( Int, TsType ))
    | Tuple (List TsType) (Maybe TsType) -- Maybe is a rest type - can be used for non-empty lists https://stackoverflow.com/a/56006703
    | Literal Encode.Value
    | TypeObject (List ( PropertyOptionality, String, TsType ))
    | ObjectWithUniformValues TsType -- https://stackoverflow.com/a/13315210
    | Union ( TsType, List TsType )
    | Unknown
    | TsNever
    | Intersection (List TsType)
