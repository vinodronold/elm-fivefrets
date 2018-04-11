port module Ports exposing (..)

import Data.Song as Song
import Json.Encode as Encode


pushDataToJS : ElmDataOut -> Cmd msg
pushDataToJS data =
    case data of
        ElmPlayerStatus ->
            elmData { tag = "ElmPlayerStatus", data = Encode.null }

        LoadYouTubeVideo youTubeID ->
            elmData { tag = "LoadYouTubeVideo", data = Encode.string <| Song.youTubeIDtoString youTubeID }


pullJSDataToElm : (JSDataIn -> msg) -> (String -> msg) -> Sub msg
pullJSDataToElm tagger onError =
    jsData <| parseJSData tagger onError


parseJSData : (JSDataIn -> msg) -> (String -> msg) -> PortData -> msg
parseJSData tagger onError js =
    case js.tag of
        "JSPlayerStatus" ->
            tagger <| JSPlayerStatus

        _ ->
            onError <| "Unexpected info from outside: " ++ toString jsData


type alias PortData =
    { tag : String
    , data : Encode.Value
    }


type JSDataIn
    = JSPlayerStatus


type ElmDataOut
    = ElmPlayerStatus
    | LoadYouTubeVideo Song.YouTubeID


port elmData : PortData -> Cmd msg


port jsData : (PortData -> msg) -> Sub msg
