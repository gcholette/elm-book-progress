module Page.UserProgression exposing (Model, Msg, init, update, view)

import Html as H exposing (Html, form, div, p, h1, a, i, text, program, button, br, table, tr, td, th, span, thead, input)
import Html.Attributes as HT exposing (class, href, type_, value, placeholder)
import Html.Events as HV exposing (on, onClick, onInput, targetValue)
import Helpers exposing (..)
import Http
import Task exposing (Task)
import Request.Book
import Data.Book as Book exposing (Book)


type alias Model =
    { errors : List String
    , title : String
    , editMode : Bool
    , books : List Book
    }



--init : ( Model, Cmd Msg )
--init =
--    ( Model "Books progression" [] [] False, getBooks )


init : Task Http.Error Model
init =
    let
        loadBooks =
            Request.Book.get
                |> Http.toTask
    in
        Task.map (Model [] "Technical books" False) loadBooks


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
        ( id, body )
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook


updateBookAuthor : ( String, String ) -> Cmd Msg
updateBookAuthor ( id, author ) =
    let
        body =
            Http.jsonBody (Book.authorJson ( id, author ))
    in
        ( id, body )
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook


updateBookLink : ( String, String ) -> Cmd Msg
updateBookLink ( id, link ) =
    let
        body =
            Http.jsonBody (Book.linkJson ( id, link ))
    in
        ( id, body )
            |> Request.Book.update
            |> Http.send HttpPostUpdateBook


updateBookProgression : ( String, Int ) -> Cmd Msg
updateBookProgression ( id, progression ) =
    let
        body =
            Http.jsonBody (Book.progressionJson ( id, progression ))
    in
        ( id, body )
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



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "book-index-title" ]
            [ text model.title ]
        , form [ class "books-list" ]
            [ (userTable model)
            , (if model.editMode then
                viewAddBtn
               else
                viewEmpty
              )
            ]
        , div
            [ onClick ToggleEditMode
            , class "edit-books-container"
            ]
            [ button [ class "edit-books-btn" ]
                [ text
                    (if (not model.editMode) then
                        "Edit books"
                     else
                        "View books"
                    )
                ]
            ]
        , p []
            [ text (String.concat model.errors) ]

        -- error message if one
        ]


viewEmpty : Html Msg
viewEmpty =
    text ""


viewAddBtn : Html Msg
viewAddBtn =
    div [ class "add-btn-section" ]
        [ button
            [ class "add-btn"
            , type_ "button"
            , onClick CreateBook
            ]
            [ text "+" ]
        ]


userTable : Model -> Html Msg
userTable model =
    table [ class "books-table" ]
        (List.concat
            [ [ thead []
                    []
              ]
            , (userTableRows model)
            ]
        )


userTableRows : Model -> List (Html Msg)
userTableRows model =
    model.books
        |> List.sortBy .progression
        |> List.reverse
        |> List.map
            (\book ->
                tr [ class "book-entry" ]
                    (if model.editMode then
                        (userTableRowEdit book)
                     else
                        (userTableRow book)
                    )
            )


userTableRow : Book -> List (Html Msg)
userTableRow book =
    [ td [ class "book-name-col" ]
        [ a [ class "book-name", href (safeString book.link) ]
            [ text (String.concat [ book.title, " - ", (safeString book.author) ]) ]
        ]
    , td [ class "book-progression-col" ]
        [ if book.progression == 100 then
            span [ class "book-completed" ]
                [ text "Completed" ]
          else
            span []
                [ text (String.concat [ (toString book.progression), "%" ]) ]
        ]
    ]


userTableRowEdit : Book -> List (Html Msg)
userTableRowEdit book =
    [ td [ class "book-name-col" ]
        [ (viewEditTitle book)
        , span [] [ text " - " ]
        , (viewEditAuthor book)
        , span [] [ text " @ " ]
        , (viewEditLink book)
        ]
    , td [ class "book-progression-col" ]
        [ div [ class "number-section" ]
            [ input
                [ type_ "number"
                , value (toString book.progression)
                , onBlurTarget (UpdateBookProgression book)
                ]
                []
            , span [ onClick (DeleteBook book) ]
                [ i [ class "fas fa-trash-alt delete-icon" ]
                    []
                ]
            ]
        ]
    ]


viewEditTitle : Book -> Html Msg
viewEditTitle book =
    input
        [ type_ "text"
        , value book.title
        , onBlurTarget (UpdateBookTitle book)
        , placeholder "Title"
        ]
        []


viewEditAuthor : Book -> Html Msg
viewEditAuthor book =
    input
        [ type_ "text"
        , value (safeString book.author)
        , onBlurTarget (UpdateBookAuthor book)
        , placeholder "Author"
        ]
        []


viewEditLink : Book -> Html Msg
viewEditLink book =
    input
        [ type_ "text"
        , value (safeString book.link)
        , onBlurTarget (UpdateBookLink book)
        , placeholder "Website"
        ]
        []
