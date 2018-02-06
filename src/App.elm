module App exposing (..)

import Http
import Data.Book as Book exposing (Book)
import Html as H exposing (Html, form, div, p, h1, a, i, text, program, button, br, table, tr, td, th, span, thead, input)
import Html.Attributes as HT exposing (class, href, type_, value, placeholder)
import Html.Events as HV exposing (on, onClick, onInput, targetValue)
import Json.Decode.Pipeline as JP exposing (decode, required, optional)
import Json.Decode as JD exposing (Decoder, at, list, field, int, string)
import Json.Encode as JE exposing (Value)

-- Main

main : Program Never Model Msg
main = 
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

apiUrl : String
apiUrl = "http://localhost:3001/"

booksUrl : String
booksUrl = apiUrl ++ "books"

-- Model

type alias Model = 
    { title : String
    , books : List Book
    , message : String
    , editMode : Bool
    }

safeString : Maybe String -> String
safeString str = Maybe.withDefault "" str

safeInt : Maybe Int -> Int
safeInt x = Maybe.withDefault 0 x

init : (Model, Cmd Msg)
init = 
    (Model "Technical Books progression" [] "" False, getBooks)

-- Messages

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

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        HttpGetBooks (Ok books) ->
            ({ model | books = books }, Cmd.none)

        HttpGetBooks (Err error) ->
            ({ model | message = toString error }, Cmd.none)

        HttpPostCreateBook (Ok book) ->
            ({ model | books = book :: model.books}, Cmd.none)

        HttpPostCreateBook (Err error) ->
            ({ model | message = toString error }, Cmd.none)

        HttpPostUpdateBook (Ok book) ->
            (model, Cmd.none)

        HttpPostUpdateBook (Err error) ->
            ({ model | message = toString error }, Cmd.none)

        HttpDeleteBook (Ok str) ->
            (model, getBooks)

        HttpDeleteBook (Err error) ->
            ({ model | message = toString error }, Cmd.none)

        ToggleEditMode ->
            ({ model | editMode = (not model.editMode)}, Cmd.none)

        CreateBook ->
            (model, postNewBook)

        UpdateBookTitle book newTitle ->
            ({ model | books = (List.map (\bk ->
                if bk == book then
                    { bk | title = newTitle}
                else
                    bk
            ) model.books) }, updateBookTitle (book.id, newTitle))

        UpdateBookAuthor book newAuthor ->
            ({ model | books = (List.map (\bk ->
                if bk == book then
                    { bk | author = Just newAuthor }
                else
                    bk
            ) model.books) }, updateBookAuthor (book.id, newAuthor))

        UpdateBookLink book newLink ->
            ({ model | books = (List.map (\bk ->
                if bk == book then
                    { bk | link = Just newLink }
                else
                    bk
            ) model.books) }, updateBookLink (book.id, newLink))

        UpdateBookProgression book newProgression ->
            let
                prog = Result.withDefault 0 (String.toInt newProgression)                
            in
                ({ model | books = (List.map (\bk ->
                    if bk == book then
                        { bk | progression = prog }
                    else
                        bk
                ) model.books) }, updateBookProgression (book.id, prog))

        DeleteBook book ->
            (model, deleteById book.id)

-- Http stuff

updateBookTitle : (String, String) -> Cmd Msg
updateBookTitle (id, title) = 
     let
         request = 
            booksPostUpdateReq (id, (Http.jsonBody (Book.titleJson (id, title)))) 
     in
        Http.send HttpPostUpdateBook request

updateBookAuthor : (String, String) -> Cmd Msg
updateBookAuthor (id, author) = 
     let
         request = 
            booksPostUpdateReq (id, (Http.jsonBody (Book.authorJson (id, author)))) 
     in
        Http.send HttpPostUpdateBook request

updateBookLink : (String, String) -> Cmd Msg
updateBookLink (id, link) = 
     let
         request = 
            booksPostUpdateReq (id, (Http.jsonBody (Book.linkJson (id, link)))) 
     in
        Http.send HttpPostUpdateBook request

updateBookProgression : (String, Int) -> Cmd Msg
updateBookProgression (id, progression) = 
     let
         request = 
            booksPostUpdateReq (id, (Http.jsonBody (Book.progressionJson (id, progression)))) 
     in
        Http.send HttpPostUpdateBook request

deleteById : String -> Cmd Msg
deleteById id =
    let
        request = bookDeleteReq id
    in
        Http.send HttpDeleteBook request  

reqHeaders : List Http.Header
reqHeaders = 
    [ Http.header "Access-Control-Allow-Origin" "*"
    ]

postNewBook : Cmd Msg
postNewBook =
    let
        request = booksPostEmptyReq
    in
        Http.send HttpPostCreateBook request

bookDeleteReq : String -> Http.Request String
bookDeleteReq id = 
    { method = "DELETE"
    , headers = reqHeaders
    , url = booksUrl ++ "/" ++ id
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

booksPostUpdateReq : (String, Http.Body) -> Http.Request Book
booksPostUpdateReq (id, json) = 
    { method = "POST"
    , headers = reqHeaders
    , url = booksUrl ++ "/" ++ id
    , body = json
    , expect = Http.expectJson Book.decoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 
    
booksPostEmptyReq : Http.Request Book
booksPostEmptyReq = 
    { method = "POST"
    , headers = reqHeaders
    , url = booksUrl
    , body = Http.emptyBody
    , expect = Http.expectJson Book.decoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

getBooks : Cmd Msg
getBooks =
    let
        request = booksGetReq
    in
        Http.send HttpGetBooks request
        
booksGetReq : Http.Request (List Book)
booksGetReq = 
    { method = "GET"
    , headers = reqHeaders 
    , url = booksUrl
    , body = Http.emptyBody
    , expect = Http.expectJson Book.listDecoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

onBlurTarget : (String -> msg) -> H.Attribute msg
onBlurTarget tagger =
    on "blur" (JD.map tagger targetValue)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

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
        , div [ onClick ToggleEditMode
              , class "edit-books-container" 
              ]
              [ button [ class "edit-books-btn" ] 
                       [ text "Edit books"] 
              ]
        , p [] 
            [ text model.message ] -- error message if one
        ]

viewEmpty : Html Msg
viewEmpty = text ""

viewAddBtn : Html Msg
viewAddBtn =
    div [ class "add-btn-section" ]
        [ button [ class "add-btn"
                 , type_ "button"
                 , onClick CreateBook 
                 ]
                 [ text "+" ]
        ]

userTable : Model -> Html Msg
userTable model =
    table [ class "books-table" ]
          ( List.concat [ [ thead [] 
                                  [] 
                          ], 
                          (userTableRows model) 
                        ]
          )

userTableRows : Model -> List (Html Msg)
userTableRows model =
    model.books
        |> List.sortBy .progression
        |> List.reverse
        |> List.map (\book -> tr [ class "book-entry" ]
                                 (if model.editMode then 
                                    (userTableRowEdit book)
                                 else
                                    (userTableRow book))
                                 ) 

userTableRow : Book -> List (Html Msg)
userTableRow book = 
    [ td [ class "book-name-col"] 
         [ a [ class "book-name", href (safeString book.link) ] 
             [ text (String.concat [book.title, " - ", (safeString book.author) ]) ] 
         ]
    , td [ class "book-progression-col" ] 
         [  
            if book.progression == 100 then 
                span [ class "book-completed" ] 
                     [ text "Completed" ]
            else 
                span [] 
                     [ text (String.concat [(toString book.progression), "%"]) ]
         ]
    ]
                    
userTableRowEdit : Book -> List (Html Msg)
userTableRowEdit book =
    [ td [ class "book-name-col" ] 
         [ (viewEditTitle book)
         , span  [] [ text " - " ]
         , (viewEditAuthor book)
         , span [] [ text " @ "]
         , (viewEditLink book) 
         ]
    , td [ class "book-progression-col" ] 
         [ div [ class "number-section"]
               [ input [ type_ "number" 
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
    input [ type_ "text"
          , value book.title 
          , onBlurTarget (UpdateBookTitle book)
          , placeholder "Title"
          ] 
          []


viewEditAuthor : Book -> Html Msg
viewEditAuthor book = 
    input [ type_ "text"
          , value (safeString book.author) 
          , onBlurTarget (UpdateBookAuthor book)
          , placeholder "Author"
          ] 
          []

viewEditLink : Book -> Html Msg
viewEditLink book = 
    input [ type_ "text"
          , value (safeString book.link) 
          , onBlurTarget (UpdateBookLink book)
          , placeholder "Website"
          ] 
          []

