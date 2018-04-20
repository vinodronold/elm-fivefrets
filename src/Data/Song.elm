module Data.Song
    exposing
        ( ModelWithSongs
        , SongID
        , Songs
        , YouTubeID
        , decodeYTItem
        , decodeYouTubeID
        , songsFeed
        , youTubeIDParser
        , youTubeIDtoString
        )

import Http
import Json.Decode as Decode
import UrlParser as Url


----------------------------------------------------------------


type alias ModelWithSongs a =
    { a
        | songs : Songs
    }


type alias Songs =
    List SongID


type alias SongID =
    { ytid : YouTubeID
    , title : String
    , imgUrl : String
    }



----------------------------------------------------------------


type YouTubeID
    = YouTubeID String


youTubeIDtoString : YouTubeID -> String
youTubeIDtoString (YouTubeID id) =
    id


youTubeIDParser : Url.Parser (YouTubeID -> a) a
youTubeIDParser =
    Url.custom "SONG" (Ok << YouTubeID)



----------------------------------------------------------------
--- REQUESTS ---


songsFeed : Http.Request (List SongID)
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


decodeYTUrl : Decode.Decoder (List SongID)
decodeYTUrl =
    decodeYTItem
        |> Decode.list
        |> Decode.field "items"


decodeYTItem : Decode.Decoder SongID
decodeYTItem =
    Decode.map3 SongID
        (Decode.at [ "id", "videoId" ] decodeYouTubeID)
        (Decode.at [ "snippet", "title" ] Decode.string)
        (Decode.at [ "snippet", "thumbnails", "default", "url" ] Decode.string)


decodeYouTubeID : Decode.Decoder YouTubeID
decodeYouTubeID =
    Decode.map YouTubeID Decode.string
