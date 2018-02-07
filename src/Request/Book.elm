module Request.Book exposing (..)

import Data.Book as Book exposing (Book)
import Http
import HttpBuilder exposing (..)
import Time exposing (Time)
import Request.Helpers exposing (apiUrl)


corsHeader : Http.Header
corsHeader =
    Http.header "Access-Control-Allow-Origin" "*"


get : Http.Request (List Book)
get =
    apiUrl "books/" 
        |> HttpBuilder.get         
        |> withHeader "Access-Control-Allow-Origin" "*"
        |> withExpect (Http.expectJson Book.listDecoder)
        |> HttpBuilder.toRequest


delete : String -> Http.Request String
delete id =
    apiUrl ("books/" ++ id)
        |> HttpBuilder.delete
        |> withHeader "Access-Control-Allow-Origin" "*"
        |> withExpect (Http.expectString)
        |> HttpBuilder.toRequest


update : ( String, Http.Body) -> Http.Request Book
update ( id, json ) =
    apiUrl ("books/" ++ id)
        |> HttpBuilder.post
        |> withHeader "Access-Control-Allow-Origin" "*"
        |> withBody json
        |> withExpect (Http.expectJson Book.decoder)
        |> HttpBuilder.toRequest


create : Http.Request Book
create =
    apiUrl "books/"
        |> HttpBuilder.post
        |> withHeader "Access-Control-Allow-Origin" "*"
        |> withExpect (Http.expectJson Book.decoder)
        |> HttpBuilder.toRequest
