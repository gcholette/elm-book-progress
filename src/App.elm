module App exposing (..)

import Html exposing (Html, form, div, p, h1, a, i, text, program, button, br, table, tr, td, th, span, thead, input)
import Http 
import Json.Decode as JD exposing (Decoder, at, list, field, int, string)
import Navigation exposing (Location)
import Page.UserProgression as UserProgression
import Route exposing (Route)
import Task

-- Main
type Page
    = Blank
    | Root
    | UserProgression UserProgression.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page


type Msg
    = SetRoute (Maybe Route)
    | UserProgressionLoaded (Result Http.Error UserProgression.Model)
    | UserProgressionMsg UserProgression.Msg


type alias Model =
    { pageState : PageState}

init : JD.Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialUserPage }


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ({ model | pageState = TransitioningFrom (getPage model.pageState) }, Task.attempt toMsg task)
            
    in
    case maybeRoute of 
        Nothing ->
         ({ model | pageState = Loaded Blank }, Cmd.none)

        Just Route.Root ->
          ({ model | pageState = Loaded Root }, Cmd.none)

        Just Route.UserProgression ->
          transition UserProgressionLoaded (UserProgression.init)
          

getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


initialPage : Page
initialPage =
    Blank


initialUserPage : Page
initialUserPage =
    UserProgression (UserProgression.Model [] "" False [])

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let 
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

    in
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( UserProgressionLoaded (Ok subModel), _ ) ->
            ({ model | pageState = Loaded (UserProgression subModel) }, Cmd.none)

        ( UserProgressionLoaded (Err err), _ ) ->
            (model, Cmd.none)

        ( UserProgressionMsg subMsg, UserProgression subModel ) ->
            toPage UserProgression UserProgressionMsg (UserProgression.update) subMsg subModel

        (_, _) ->
            (model, Cmd.none)

            



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage False page

        TransitioningFrom page ->
            viewPage True page


viewPage : Bool -> Page -> Html Msg
viewPage isLoading page =
    case page of
        Blank ->
            text ""

        Root ->
            text ""

        UserProgression subModel ->
            UserProgression.view subModel
                |> Html.map UserProgressionMsg


main : Program JD.Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }