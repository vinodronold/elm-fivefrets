module Page.Loading exposing (..)

import Element as E
import Element.Attributes as A
import Styles as S
import Time exposing (Time)


type Msg
    = Tick Time


type alias Model =
    { count : Int
    }


getInitModel : Model
getInitModel =
    Model 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ ->
            { model | count = model.count + 1 }


loading : Model -> E.Element S.Styles variation msg
loading { count } =
    E.el S.None [ A.center, A.verticalCenter, A.padding 20 ] <|
        E.column S.None
            [ A.center, A.spacing 10 ]
            [ E.row S.None
                [ A.spacing 5, A.verticalCenter, A.height <| A.px 30 ]
                [ loadingBox <| (count % 9 == 0 || (count + 1) % 9 == 0)
                , loadingBox <| (count % 9 == 1 || (count + 1) % 9 == 1)
                , loadingBox <| (count % 9 == 2 || (count + 1) % 9 == 2)
                , loadingBox <| (count % 9 == 3 || (count + 1) % 9 == 3)
                , E.el S.None [ A.verticalCenter, A.height <| A.px 20 ] <| E.text "Loading"
                , loadingBox <| (count % 9 == 4 || (count + 1) % 9 == 4)
                , loadingBox <| (count % 9 == 5 || (count + 1) % 9 == 5)
                , loadingBox <| (count % 9 == 6 || (count + 1) % 9 == 6)
                , loadingBox <| (count % 9 == 7 || (count + 1) % 9 == 7)
                ]
            ]


loadingBox : Bool -> E.Element S.Styles variation msg
loadingBox b =
    E.el S.LoadingBox
        [ A.width <| A.px 5
        , A.height <| A.px <| getBoxHeight b
        ]
        E.empty


getBoxHeight : Bool -> Float
getBoxHeight b =
    if b then
        20
    else
        10
