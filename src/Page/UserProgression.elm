module Page.UserProgression exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, text)

type alias Model =
    { errors : List String
    }

init : ( Model, Cmd Msg )
init =
    ( Model [], Cmd.none)

view : Model -> Html Msg
view model = div [] [ text "wow dude" ]

type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)