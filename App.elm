module App exposing (..)

import Http
import Html as H exposing (Html, form, div, p, h1, a, text, program, button, br, table, tr, td, th, span, thead, input)
import Html.Attributes as HT exposing (class, href, type_, value)
import Html.Events as HV exposing (onClick)
import Json.Decode.Pipeline as JP exposing (decode, required, optional)
import Json.Decode as JD exposing (Decoder, at, list, field, int, string)

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

type alias Book =
    { id : String
    , title : String
    , author : Maybe String
    , link : Maybe String
    , progression : Int
    }    

safeString : Maybe String -> String
safeString str =   
    case str of 
    Just s  -> s
    Nothing -> ""

safeInt : Maybe Int -> Int
safeInt x =
    case x of 
    Just x  -> x
    Nothing -> 0

init : (Model, Cmd Msg)
init = 
    (Model "Technical Books progression" [] "" False, getBooks)

-- Messages

type Msg 
    = HttpGetBooks (Result Http.Error (List Book))
    | HttpPostCreateBook (Result Http.Error Book)
    | ToggleEditMode
    | CreateBook

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

        ToggleEditMode ->
            ({ model | editMode = (not model.editMode)}, Cmd.none)

        CreateBook ->
            (model, postNewBook)

-- Http stuff

postNewBook : Cmd Msg
postNewBook =
    let
        request = booksPostReq
    in
        Http.send HttpPostCreateBook request

booksPostReq : Http.Request Book
booksPostReq = 
    { method = "POST"
    , headers = [ Http.header "Access-Control-Allow-Origin" "*"
                , Http.header "Content-type" "application/json"
                ]
    , url = booksUrl
    , body = Http.emptyBody
    , expect = Http.expectJson bookDecoder
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
    , headers = [ Http.header "Access-Control-Allow-Origin" "*"
                , Http.header "Content-type" "application/json"
                ]
    , url = booksUrl
    , body = Http.emptyBody
    , expect = Http.expectJson booksDecoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

bookDecoder : Decoder Book
bookDecoder =
    JP.decode Book
        |> required "_id"  string
        |> required "title" string
        |> optional "author" (JD.map Just string) Nothing
        |> optional "link" (JD.map Just string) Nothing
        |> required "progression" int


booksDecoder : Decoder (List Book)
booksDecoder = 
    list bookDecoder


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
    div []
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
         [ input [ type_ "text", value book.title ] 
                 []
         , span  [] 
                 [ text " - " ]
         , input [ type_ "text", value (safeString book.author) ] 
                 []
         ]
    , td [ class "book-progression-col" ] 
         [
           input [ type_ "number", value (toString book.progression) ]
                 []
         ]
    ]