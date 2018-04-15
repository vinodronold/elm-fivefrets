module Page.Player exposing (Model, Msg(..), load, update, view)

import Data.ChordTime as ChordTime exposing (ChordTime)
import Data.Player as Player exposing (PlayerStatus, YTPlayerID)
import Data.Song as Song exposing (SongID, YouTubeID)
import Element as E
import Element.Attributes as A
import Page.Errored as Errored
import Ports
import Styles as S
import Task exposing (Task)
import Time exposing (Time)
import View.Songs as ViewSong
import View.Utils as Utils


type alias Model =
    { id : SongID
    , playerID : YTPlayerID
    , playerStatus : PlayerStatus
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    }


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
            (Player.YTPlayerID "YT_Player")
            Player.NotLoaded
            []
            Nothing
            ChordTime.sample


type Msg
    = Load YTPlayerID YouTubeID
    | ChangePlayerStatus PlayerStatus
    | UpdatePlayerStatus PlayerStatus
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            { model | playerStatus = Player.Playing } ! []

        Load playerID youTubeID ->
            model ! [ Ports.pushDataToJS <| Ports.LoadYouTubeVideo playerID youTubeID ]

        ChangePlayerStatus playerStatus ->
            model ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus ]

        UpdatePlayerStatus playerStatus ->
            { model | playerStatus = playerStatus } ! []


view : Model -> E.Element S.Styles variation Msg
view model =
    E.column S.None
        []
        [ displayChords model
        , displayControl model
        , diplayYouTubeVideo model
        ]


displayControl : Model -> E.Element S.Styles variation Msg
displayControl model =
    E.row S.None [ A.spacing 10, A.padding 20, A.center ] <| controlButtons model


controlButtons : Model -> List (E.Element S.Styles variation Msg)
controlButtons { id, playerID, playerStatus } =
    case playerStatus of
        Player.NotLoaded ->
            [ Utils.button "Play" (Load playerID id.ytid)
            , Utils.disabledButton "Stop"
            ]

        Player.Playing ->
            [ pauseButton, stopButton ]

        Player.Paused ->
            [ playButton, stopButton ]

        Player.Ended ->
            [ playButton
            , Utils.disabledButton "Stop"
            ]

        Player.Cued ->
            [ playButton
            , Utils.disabledButton "Stop"
            ]

        _ ->
            [ Utils.disabledButton "Play"
            , Utils.disabledButton "Stop"
            ]


playButton : E.Element S.Styles variation Msg
playButton =
    Utils.button "Play" (ChangePlayerStatus Player.Playing)


pauseButton : E.Element S.Styles variation Msg
pauseButton =
    Utils.button "Pause" (ChangePlayerStatus Player.Paused)


stopButton : E.Element S.Styles variation Msg
stopButton =
    Utils.button "Stop" (ChangePlayerStatus Player.Ended)


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


diplayYouTubeVideo : Model -> E.Element S.Styles variation msg
diplayYouTubeVideo { id, playerID } =
    let
        pad =
            10

        ht =
            180

        wd =
            320
    in
    E.screen <|
        E.el S.YouTubeSpace
            [ A.id <| Player.ytPlayerIDToString playerID
            , A.alignBottom
            , A.padding pad
            , A.height <| A.px <| ht + pad * 2
            , A.width <| A.px <| wd + pad * 2
            ]
        <|
            ViewSong.displaySongImg id
