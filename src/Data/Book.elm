module Data.Book exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode 

type alias Book =
    { id : String
    , title : String
    , author : Maybe String
    , link : Maybe String
    , progression : Int
    }    


decoder : Decoder Book
decoder =
    decode Book
        |> required "_id"  Decode.string
        |> required "title" Decode.string
        |> optional "author" (Decode.map Just Decode.string) Nothing
        |> optional "link" (Decode.map Just Decode.string) Nothing
        |> required "progression" Decode.int
    
listDecoder : Decoder (List Book)
listDecoder = 
    Decode.list decoder

titleJson : (String, String) -> Encode.Value
titleJson (id, title) =
    Encode.object
      [ ("_id", Encode.string id)   
      , ("title", Encode.string title)   
      ]


authorJson : (String, String) -> Encode.Value
authorJson (id, author) =
    Encode.object
      [ ("_id", Encode.string id)
      , ("author", Encode.string author)   
      ]

linkJson : (String, String) -> Encode.Value
linkJson (id, link) =
    Encode.object
      [ ("_id", Encode.string id)
      , ("link", Encode.string link)   
      ]

progressionJson : (String, Int) -> Encode.Value
progressionJson (id, progression) =
    Encode.object
      [ ("_id", Encode.string id)
      , ("progression", Encode.int progression)   
      ]