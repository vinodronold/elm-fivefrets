port module Ports exposing (..)

import Data.Player as Player
import Data.Song as Song
import Json.Decode as Decode
import Json.Encode as Encode


pushDataToJS : ElmDataOut -> Cmd msg
pushDataToJS data =
    case data of
        LoadYouTubeVideo playerID youTubeID ->
            elmData
                { tag = "LoadYouTubeVideo"
                , data =
                    Encode.object
                        [ ( "youTubeID", Encode.string <| Song.youTubeIDtoString youTubeID )
                        , ( "playerID", Encode.string <| Player.ytPlayerIDToString playerID )
                        ]
                }

        SetPlayerState playerStatus ->
            case playerStatus of
                Player.Playing ->
                    elmData { tag = "PlayVideo", data = Encode.null }

                Player.Paused ->
                    elmData { tag = "PauseVideo", data = Encode.null }

                Player.Ended ->
                    elmData { tag = "StopVideo", data = Encode.null }

                _ ->
                    elmData { tag = "SetPlayerState_NoOp", data = Encode.null }


pullJSDataToElm : (JSDataIn -> msg) -> (String -> msg) -> Sub msg
pullJSDataToElm tagger onError =
    jsData <| parseJSData tagger onError


parseJSData : (JSDataIn -> msg) -> (String -> msg) -> PortData -> msg
parseJSData tagger onError js =
    case js.tag of
        "JSPlayerStatus" ->
            case Decode.decodeValue Decode.int js.data of
                Ok jsPlayerStatus ->
                    tagger <| JSPlayerStatus jsPlayerStatus

                Err e ->
                    onError e

        _ ->
            onError <| "Unexpected info from outside: " ++ toString jsData


type alias PortData =
    { tag : String
    , data : Encode.Value
    }


type JSDataIn
    = JSPlayerStatus Int


type ElmDataOut
    = LoadYouTubeVideo Player.YTPlayerID Song.YouTubeID
    | SetPlayerState Player.PlayerStatus


port elmData : PortData -> Cmd msg


port jsData : (PortData -> msg) -> Sub msg
