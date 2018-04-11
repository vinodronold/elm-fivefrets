module Page.Player exposing (Model, Msg(..), PlayerStatus(..), load, update, view)

import Data.ChordTime as ChordTime exposing (ChordTime)
import Data.Song as Song exposing (SongID, YouTubeID)
import Element as E
import Element.Attributes as A
import Page.Errored as Errored
import Styles as S
import Task exposing (Task)
import Time exposing (Time)
import View.Songs as ViewSong
import View.Utils as Utils


type alias Model =
    { id : SongID
    , playerStatus : PlayerStatus
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    }


type PlayerStatus
    = NotStarted
    | Playing
    | Paused
    | Stopped


load : YouTubeID -> Task Errored.PageLoadError Model
load youTubeID =
    -- let
    --     loadPlayer =
    --         playerData
    --             |> Http.toTask
    --     handleLoadError _ =
    --         PageLoadError "Chords Player is currently unavailable."
    -- in
    Task.succeed <|
        Model { ytid = youTubeID, title = "PLAYER SONG", imgUrl = "https://i.ytimg.com/vi/NCtzkaL2t_Y/mqdefault.jpg" }
            NotStarted
            []
            Nothing
            ChordTime.sample


type Msg
    = ChangePlayerStatus PlayerStatus
    | Tick Time


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick _ ->
            { model | playerStatus = Playing }

        ChangePlayerStatus playerStatus ->
            { model | playerStatus = playerStatus }


view : Model -> E.Element S.Styles variation Msg
view model =
    E.column S.None
        []
        [ displayChords model
        , displayControl model.playerStatus
        , diplayYouTubeVideo model.id
        ]


displayControl : PlayerStatus -> E.Element S.Styles variation Msg
displayControl playerStatus =
    E.row S.None [ A.spacing 10, A.padding 20, A.center ] <| controlButtons playerStatus


controlButtons : PlayerStatus -> List (E.Element S.Styles variation Msg)
controlButtons playerStatus =
    case playerStatus of
        NotStarted ->
            [ Utils.button "Play" (ChangePlayerStatus Playing)
            , Utils.disabledButton "Stop"
            ]

        Playing ->
            [ Utils.button "Pause" (ChangePlayerStatus Paused)
            , Utils.button "Stop" (ChangePlayerStatus Stopped)
            ]

        Paused ->
            [ Utils.button "Play" (ChangePlayerStatus Playing)
            , Utils.button "Stop" (ChangePlayerStatus Stopped)
            ]

        Stopped ->
            [ Utils.button "Play" (ChangePlayerStatus Playing)
            , Utils.disabledButton "Stop"
            ]


displayChords : Model -> E.Element S.Styles variation msg
displayChords playerModel =
    E.grid S.None
        [ A.spacing 10 ]
        { columns = List.repeat 8 A.fill
        , rows = []
        , cells = chordsGridCells playerModel
        }


chordsGridCells : Model -> List (E.OnGrid (E.Element S.Styles variation msg))
chordsGridCells { playedChords, currChord, nextChords } =
    let
        played =
            List.indexedMap (mapChords S.Inactive 0) playedChords

        ( nextStart, curr ) =
            case currChord of
                Nothing ->
                    ( List.length playedChords
                    , []
                    )

                Just chord ->
                    ( List.length playedChords + 1
                    , List.indexedMap (mapChords S.Active <| List.length playedChords) <| chord :: []
                    )

        next =
            List.indexedMap (mapChords S.Inactive nextStart) nextChords
    in
    played ++ curr ++ next


mapChords : S.ActiveInactive -> Int -> Int -> ChordTime -> E.OnGrid (E.Element S.Styles variation msg)
mapChords activeInactive start idx ( chord, time ) =
    E.cell
        { start = ( (start + idx) % 8, (start + idx) // 8 )
        , width = 1
        , height = 1
        , content =
            E.el (S.ChordItem activeInactive)
                [ A.paddingXY 0 10
                , A.inlineStyle [ ( "text-align", "center" ) ]
                ]
                (E.text <| ChordTime.chordName chord)
        }


diplayYouTubeVideo : SongID -> E.Element S.Styles variation msg
diplayYouTubeVideo song =
    E.screen <|
        E.el S.YouTubeSpace [ A.id "YT_Player", A.alignBottom, A.padding 10 ] <|
            ViewSong.displaySongImg song
