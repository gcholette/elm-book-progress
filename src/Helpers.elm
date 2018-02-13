module Helpers exposing (..)

import Json.Decode as JD exposing (Decoder, at, list, field, int, string)
import Html exposing (Html, Attribute)
import Html.Events exposing (on, targetValue)


safeString : Maybe String -> String
safeString str =
    Maybe.withDefault "" str


safeInt : Maybe Int -> Int
safeInt x =
    Maybe.withDefault 0 x


onBlurTarget : (String -> msg) -> Attribute msg
onBlurTarget tagger =
    on "blur" (JD.map tagger targetValue)
