module View.Songs exposing (..)

import Data.Song as Data
import Element as E
import Element.Attributes as A
import Route
import Styles as S


songs : Data.ModelWithSongs a -> E.Element S.Styles variation msg
songs model =
    E.column S.None [ A.spacing 10 ] <|
        List.map displaySong model.songs


displaySong : Data.SongID -> E.Element S.Styles variation msg
displaySong song =
    E.link (Route.href <| Route.Player song.id) <|
        E.row S.SongItem
            [ A.spacing 20 ]
            [ E.image S.None [] { src = song.imgUrl, caption = song.title }
            , E.column S.None
                [ A.spacing 5, A.verticalCenter ]
                [ E.el S.None [] <| E.text song.title
                ]
            ]
