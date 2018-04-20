module Data.Player exposing (..)

import Data.Song as Song exposing (SongID, YouTubeID)
import Http
import Json.Decode as Decode


type PlayerStatus
    = NotLoaded
    | UnStarted
    | Ended
    | Playing
    | Paused
    | Buffering
    | Cued


jsToElmPlayerStatus : Int -> PlayerStatus
jsToElmPlayerStatus i =
    case i of
        (-1) ->
            UnStarted

        0 ->
            Ended

        1 ->
            Playing

        2 ->
            Paused

        3 ->
            Buffering

        5 ->
            Cued

        _ ->
            NotLoaded


type YTPlayerID
    = YTPlayerID String


ytPlayerIDToString : YTPlayerID -> String
ytPlayerIDToString (YTPlayerID str) =
    str


videoDetails : YouTubeID -> Http.Request SongID
videoDetails youTubeID =
    let
        ytApiKey =
            "AIzaSyDt03O45GRK2doERZICfzCgUbeXVFtLpiY"

        ytApiUrl =
            "https://www.googleapis.com/youtube/v3/videos?part=snippet&id=" ++ Song.youTubeIDtoString youTubeID ++ "&key=" ++ ytApiKey

        request =
            Http.get ytApiUrl decodeYTVideoUrl
    in
    request


decodeYTVideoUrl : Decode.Decoder SongID
decodeYTVideoUrl =
    decodeYTVideoItem
        |> Decode.index 0
        |> Decode.field "items"


decodeYTVideoItem : Decode.Decoder SongID
decodeYTVideoItem =
    Decode.map3 SongID
        (Decode.at [ "id" ] Song.decodeYouTubeID)
        (Decode.at [ "snippet", "title" ] Decode.string)
        (Decode.at [ "snippet", "thumbnails", "medium", "url" ] Decode.string)
