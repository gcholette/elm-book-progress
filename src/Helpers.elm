module Helpers exposing (..)
 
safeString : Maybe String -> String
safeString str =
    Maybe.withDefault "" str


safeInt : Maybe Int -> Int
safeInt x =
    Maybe.withDefault 0 x

