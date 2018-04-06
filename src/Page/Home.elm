module Page.Home exposing (Model, load, view)

import Data.Song as Data
import Element as E
import Http
import Page.Errored as Errored
import Styles as S
import Task exposing (Task)
import View.Songs as View


type alias Model =
    { songs : Data.Songs
    }



--- TASK ---


load : Task Errored.PageLoadError Model
load =
    let
        loadSongs =
            Data.songsFeed
                |> Http.toTask

        handleLoadError _ =
            Errored.pageLoadError "Homepage is currently unavailable."
    in
    Task.map Model loadSongs
        |> Task.mapError handleLoadError



--- VIEW ---


view : Model -> E.Element S.Styles variation msg
view =
    View.songs
