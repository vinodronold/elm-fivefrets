port module App.Ports exposing (..)

import Json.Encode as Encode


sendDataToJS : DataToJS -> Cmd msg
sendDataToJS toJS =
    case toJS of
        ToJSTest ->
            dataToJS { tag = "TestDataToJS", data = Encode.null }


getDataFromJS : (DataFromJS -> msg) -> (String -> msg) -> Sub msg
getDataFromJS tagger onError =
    dataFromJS <| parseDataFromJS tagger onError


parseDataFromJS : (DataFromJS -> msg) -> (String -> msg) -> JSData -> msg
parseDataFromJS tagger onError jsData =
    case jsData.tag of
        "TestFromJS" ->
            tagger <| FromJSTest

        _ ->
            onError <| "Unexpected info from outside: " ++ toString jsData


type alias JSData =
    { tag : String
    , data : Encode.Value
    }


type DataToJS
    = ToJSTest


type DataFromJS
    = FromJSTest


port dataToJS : JSData -> Cmd msg


port dataFromJS : (JSData -> msg) -> Sub msg
