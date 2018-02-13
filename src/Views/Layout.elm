module Views.Layout exposing (frame)

import Html exposing (..)
import Html.Attributes exposing (..)

frame : Bool ->  Html msg -> Html msg
frame isLoading content =
    div [ class "page-frame" ]
        [ content ]

