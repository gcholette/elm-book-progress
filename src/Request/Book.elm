module Request.Book exposing (..)

import Data.Book as Book exposing (Book)
import Http
import Request.Helpers exposing (apiUrl)

reqHeaders : List Http.Header
reqHeaders = 
    [ Http.header "Access-Control-Allow-Origin" "*" ]

get : Http.Request (List Book)
get = 
    { method = "GET"
    , headers = reqHeaders 
    , url = apiUrl "books/"
    , body = Http.emptyBody
    , expect = Http.expectJson Book.listDecoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

delete : String -> Http.Request String
delete id = 
    { method = "DELETE"
    , headers = reqHeaders
    , url = apiUrl ("books/" ++ id)
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 

update : (String, Http.Body) -> Http.Request Book
update (id, json) = 
    { method = "POST"
    , headers = reqHeaders
    , url = apiUrl ("books/" ++ id)
    , body = json
    , expect = Http.expectJson Book.decoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 
    
emptyUpdate : Http.Request Book
emptyUpdate = 
    { method = "POST"
    , headers = reqHeaders
    , url = apiUrl "books/"
    , body = Http.emptyBody
    , expect = Http.expectJson Book.decoder
    , timeout = Nothing
    , withCredentials = False
    } 
    |> Http.request 