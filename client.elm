module App exposing (..)

import Http
import Html as H exposing (Html, div, p, h1, a, text, program, button, br, table, tr, td, th, span, thead)
import Html.Attributes as HT exposing (class, href)
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
    }

type alias Book =
    { id : String
    , title : String
    , link : Maybe String
    , progression : Int
    , author : Maybe String
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
    JP.decode Book
        |> required "id"  string
        |> required "title" string
        |> optional "link" (JD.map Just string) Nothing
        |> required "progression" int
        |> optional "author" (JD.map Just string) Nothing


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
    |> List.sortBy .progression
    |> List.reverse
    |> List.map (\x -> tr [ class "book-entry"] 
                    [ td [ class "book-name-col"] 
                         [ a [ class "book-name", href (safeString x.link) ] 
                             [ text (String.concat [x.title, " - ", (safeString x.author) ]) ]
                         ]
                    , td [ class "book-progression-col" ] 
                         [  
                            if x.progression == 100 then 
                                span [class "book-completed"] [ text "Completed" ]
                            else 
                                span [] [text (String.concat [(toString x.progression), "%"]) ]
                         ]
                    ] ) 
