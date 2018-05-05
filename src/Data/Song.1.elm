module Data.Song
    exposing
        ( ModelWithSongs
        , Songs
        , Song
        , YouTubeID
        , decodeYTItem
        , songsFeed
        )

import Http
import Dict exposing (Dict)
import Json.Decode as Decode
import Data.ChordTime as ChordTime exposing (ChordTime)


----------------------------------------------------------------


type alias ModelWithSongs a =
    { a
        | songs : Songs
    }


type alias Songs =
    Dict YouTubeID Song


type alias Song =
    { id : YouTubeID
    , title : String
    , imgUrlDefault : URL
    , imgUrlMedium : URL
    , chords : Maybe (List ChordTime)
    }


type alias YouTubeID =
    String


type alias URL =
    String



--- REQUESTS ---


songsFeed : Http.Request (List Song)
songsFeed =
    let
        searchText =
            "Beatles"

        ytApiKey =
            "AIzaSyDt03O45GRK2doERZICfzCgUbeXVFtLpiY"

        ytApiUrl =
            "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=5&type=video&q=" ++ searchText ++ "&key=" ++ ytApiKey

        request =
            Http.get ytApiUrl decodeYTUrl
    in
        request


decodeYTUrl : Decode.Decoder (List Song)
decodeYTUrl =
    decodeYTItem
        |> Decode.list
        |> Decode.field "items"


decodeYTItem : Decode.Decoder Song
decodeYTItem =
    Decode.map5 Song
        (Decode.at [ "id", "videoId" ] Decode.string)
        (Decode.at [ "snippet", "title" ] Decode.string)
        (Decode.at [ "snippet", "thumbnails", "default", "url" ] Decode.string)
        (Decode.at [ "snippet", "thumbnails", "medium", "url" ] Decode.string)
        (Decode.succeed Nothing)
