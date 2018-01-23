module App exposing (..)

import Http
import Html as H exposing (Html, div, p, h1, a, text, program, button, br, table, tr, td, th, span, thead)
import Html.Attributes as HT exposing (class, href)
import Json.Decode as JD exposing (Decoder, at, list, map2, field, int, string)

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
apiUrl = "http://localhost:3000/"

booksUrl : String
booksUrl = apiUrl ++ "books.json"

-- Model

type alias Model = 
    { title : String
    , books : List Book
    , message : String
    }

type alias Book =
    { id : Int
    , title : String
    , link : String
    , progress : Int
    , author : String
    }    

init : (Model, Cmd Msg)
init = 
    (Model "Technical Books progression" [] "", getBooks)

-- Messages

type Msg 
    = NoOp
    | HttpGetBooks (Result Http.Error (List Book))

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        NoOp ->
            (model, Cmd.none)

        HttpGetBooks (Ok books) ->
            ({ model | books = books }, Cmd.none)

        HttpGetBooks (Err error) ->
            ({ model | message = toString error }, Cmd.none)


-- Http stuff

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
    JD.map5 Book
        (field "id" int)
        (field "title" string)
        (field "link" string)
        (field "progress" int)
        (field "author" string)

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
        [ h1 [ class "book-index-title" ] [ text model.title ]
        , div [ class "books-list" ] 
              [ table [ class "books-table" ] 
                      ( List.concat [ [ thead [] [] ] , (userTableRow model) ])
              ]
        , p [] [ text model.message ]
        ]

userTableRow : Model -> List (Html Msg)
userTableRow model = 
    model.books
    |> List.sortBy .progress
    |> List.reverse
    |> List.map (\x -> tr [ class "book-entry"] 
                    [ td [ class "book-name-col"] 
                         [ a [ class "book-name", href x.link ] 
                             [ text (String.concat [(toString x.title), " - ", (toString x.author) ]) ]
                         ]
                    , td [ class "book-progression-col" ] 
                         [  
                            if x.progress == 100 then 
                                span [class "book-completed"] [ text "Completed" ]
                            else 
                                span [] [text (String.concat [(toString x.progress), "%"]) ]
                         ]
                    ] ) 
