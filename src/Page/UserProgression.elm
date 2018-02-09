module Page.UserProgression exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, text)
import Http
import Request.Book
import Data.Book as Book exposing (Book)

type alias Model =
    { errors : List String
    , books : List Book
    , editMode : Bool
    }

init : ( Model, Cmd Msg )
init =
    ( Model [] [] False, getBooks )

view : Model -> Html Msg
view model = div [] [ text "wow dude" ]

type Msg
    = HttpGetBooks (Result Http.Error (List Book))
    | HttpPostCreateBook (Result Http.Error Book)
    | HttpPostUpdateBook (Result Http.Error Book)
    | HttpDeleteBook (Result Http.Error String)
    | ToggleEditMode
    | CreateBook
    | DeleteBook Book
    | UpdateBookLink Book String
    | UpdateBookTitle Book String
    | UpdateBookAuthor Book String
    | UpdateBookProgression Book String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HttpGetBooks (Ok books) ->
            ( { model | books = books }, Cmd.none )

        HttpGetBooks (Err error) ->
            ( { model | errors = (toString error) :: model.errors }, Cmd.none )

        HttpPostCreateBook (Ok book) ->
            ( { model | books = book :: model.books }, Cmd.none )

        HttpPostCreateBook (Err error) ->
            ( { model | errors = (toString error) :: model.errors }, Cmd.none )

        HttpPostUpdateBook (Ok book) ->
            ( model, Cmd.none )

        HttpPostUpdateBook (Err error) ->
            ( { model | errors = (toString error) :: model.errors }, Cmd.none )

        HttpDeleteBook (Ok str) ->
            ( model, getBooks )

        HttpDeleteBook (Err error) ->
            ( { model | errors = (toString error) :: model.errors }, Cmd.none )

        ToggleEditMode ->
            ( { model | editMode = (not model.editMode) }, Cmd.none )

        CreateBook ->
            ( model, postNewBook )

        UpdateBookTitle book newTitle ->
            ( { model
                | books =
                    (List.map
                        (\bk ->
                            if bk == book then
                                { bk | title = newTitle }
                            else
                                bk
                        )
                        model.books
                    )
              }
            , updateBookTitle ( book.id, newTitle )
            )

        UpdateBookAuthor book newAuthor ->
            ( { model
                | books =
                    (List.map
                        (\bk ->
                            if bk == book then
                                { bk | author = Just newAuthor }
                            else
                                bk
                        )
                        model.books
                    )
              }
            , updateBookAuthor ( book.id, newAuthor )
            )

        UpdateBookLink book newLink ->
            ( { model
                | books =
                    (List.map
                        (\bk ->
                            if bk == book then
                                { bk | link = Just newLink }
                            else
                                bk
                        )
                        model.books
                    )
              }
            , updateBookLink ( book.id, newLink )
            )

        UpdateBookProgression book newProgression ->
            let
                prog =
                    Result.withDefault 0 (String.toInt newProgression)
            in
                ( { model
                    | books =
                        (List.map
                            (\bk ->
                                if bk == book then
                                    { bk | progression = prog }
                                else
                                    bk
                            )
                            model.books
                        )
                  }
                , updateBookProgression ( book.id, prog )
                )

        DeleteBook book ->
            ( model, deleteById book.id )


-- Http stuff


updateBookTitle : ( String, String ) -> Cmd Msg
updateBookTitle ( id, title ) =
    let
        body =
            Http.jsonBody (Book.titleJson ( id, title )) 
    in
        (id, body)
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook 

updateBookAuthor : ( String, String ) -> Cmd Msg
updateBookAuthor ( id, author ) =
    let
        body =
            Http.jsonBody (Book.authorJson ( id, author )) 
    in
        (id, body)
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook 


updateBookLink : ( String, String ) -> Cmd Msg
updateBookLink ( id, link ) =
    let
        body =
            Http.jsonBody (Book.linkJson ( id, link )) 
    in
        (id, body)
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook 



updateBookProgression : ( String, Int ) -> Cmd Msg
updateBookProgression ( id, progression ) =
    let 
        body = 
            Http.jsonBody (Book.progressionJson ( id, progression ))

    in
        (id, body)
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook 


deleteById : String -> Cmd Msg
deleteById id =
    id
        |> Request.Book.delete
        |> Http.send HttpDeleteBook


postNewBook : Cmd Msg
postNewBook =
    Request.Book.create
        |> Http.send HttpPostCreateBook


getBooks : Cmd Msg
getBooks =
    Request.Book.get
        |> Http.send HttpGetBooks
