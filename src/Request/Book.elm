module Request.Book exposing (..)

import Data.Book as Book exposing (Book)
import Http
import HttpBuilder exposing (..)
import Time exposing (Time)
import Request.Helpers exposing (apiUrl)


corsHeader : String
corsHeader =
    "Access-Control-Allow-Origin"


get : Http.Request (List Book)
get =
    apiUrl "books/"
        |> HttpBuilder.get
        |> withHeader corsHeader "*"
        |> withExpect (Http.expectJson Book.listDecoder)
        |> HttpBuilder.toRequest


delete : String -> Http.Request String
delete id =
    apiUrl ("books/" ++ id)
        |> HttpBuilder.delete
        |> withHeader corsHeader "*"
        |> withExpect (Http.expectString)
        |> HttpBuilder.toRequest


update : ( String, Http.Body ) -> Http.Request Book
update ( id, json ) =
    apiUrl ("books/" ++ id)
        |> HttpBuilder.post
        |> withHeader corsHeader "*"
        |> withBody json
        |> withExpect (Http.expectJson Book.decoder)
        |> HttpBuilder.toRequest


create : Http.Request Book
create =
    apiUrl "books/"
        |> HttpBuilder.post
        |> withHeader corsHeader "*"
        |> withExpect (Http.expectJson Book.decoder)
        |> HttpBuilder.toRequest
