module Page.Player exposing (Model, Msg(..), load, update, view)

import Data.ChordTime as ChordTime exposing (ChordTime)
import Data.Player as Player exposing (PlayerStatus, YTPlayerID)
import Data.Song as Song exposing (SongID, YouTubeID)
import Dom
import Dom.Scroll as Scroll
import Element as E
import Element.Attributes as A
import Element.Events as Events
import Maybe
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
    , playerTime : Maybe Time
    , scrollPos : Float
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
            Nothing
            0
            []
            Nothing
            ChordTime.sample


type Msg
    = Load YTPlayerID YouTubeID
    | ChangePlayerStatus PlayerStatus
    | UpdatePlayerStatus PlayerStatus
    | UpdatePlayerTime Time
    | SeekToPosition Time
    | Tick Time
    | ScrollingToY Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            model ! [ Ports.pushDataToJS Ports.GetPlayerCurrTime ]

        Load playerID youTubeID ->
            model ! [ Ports.pushDataToJS <| Ports.LoadYouTubeVideo playerID youTubeID ]

        ChangePlayerStatus playerStatus ->
            if playerStatus == Player.Ended then
                let
                    next =
                        case model.currChord of
                            Nothing ->
                                model.nextChords

                            Just curr ->
                                model.playedChords ++ (curr :: model.nextChords)
                in
                { model
                    | playedChords = []
                    , currChord = Nothing
                    , nextChords = next
                }
                    ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus, scrollToY 0 ]
            else
                model ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus ]

        UpdatePlayerStatus playerStatus ->
            { model | playerStatus = playerStatus } ! []

        UpdatePlayerTime playerTime ->
            let
                getplayed =
                    case model.currChord of
                        Nothing ->
                            []

                        Just chord ->
                            model.playedChords ++ chord :: []

                ( played, curr, next, cmd ) =
                    if playerTime > ChordTime.getTime model.currChord then
                        case model.nextChords of
                            x :: xs ->
                                ( getplayed
                                , Just x
                                , xs
                                , [ scrolling model.scrollPos <| List.length model.playedChords + 1 ]
                                )

                            [] ->
                                -- Chords ENDED
                                ( []
                                , Nothing
                                , getplayed
                                , [ Ports.pushDataToJS <| Ports.SetPlayerState Player.Ended, scrollToY 0 ]
                                )
                    else
                        ( model.playedChords
                        , model.currChord
                        , model.nextChords
                        , [ Cmd.none ]
                        )
            in
            { model
                | playerTime = Just playerTime
                , playedChords = played
                , currChord = curr
                , nextChords = next
            }
                ! cmd

        SeekToPosition seekToTime ->
            model ! [ Ports.pushDataToJS <| Ports.SeekTo seekToTime ]

        ScrollingToY currScrollPos ->
            { model | scrollPos = currScrollPos } ! []


scrolling : Float -> Int -> Cmd Msg
scrolling currPos totalChordsPlayed =
    if totalChordsPlayed > 8 then
        if totalChordsPlayed % 8 == 0 then
            scrollToY (currPos + 55)
        else
            Cmd.none
    else
        Cmd.none


scrollToY : Float -> Cmd Msg
scrollToY pos =
    Scroll.toY diplayChordID pos
        |> Task.attempt (always (ScrollingToY pos))


view : Model -> E.Element S.Styles variation Msg
view model =
    E.column S.None
        []
        [ displayTitle model
        , displayChords model
        , displayControl model
        , clearSpaceYouTubeVideo propsYouTubeDisplay
        , diplayYouTubeVideo model propsYouTubeDisplay
        ]



-----------------------------------------------------------------------------------


displayTitle : Model -> E.Element S.Styles variation msg
displayTitle { id } =
    E.h1 S.Title [ A.paddingXY 10 20 ] <| E.text id.title



-----------------------------------------------------------------------------------


diplayChordID : Dom.Id
diplayChordID =
    "diplayChordID"


displayChords : Model -> E.Element S.Styles variation Msg
displayChords playerModel =
    E.el S.None
        [ A.height <| A.px 150
        , A.clip
        , A.id diplayChordID
        , A.inlineStyle [ ( "scroll-behavior", "smooth" ) ]
        ]
    <|
        E.grid S.ChordGridContainer
            [ A.spacing 10
            , A.padding 10

            -- , A.moveUp <| toFloat (List.length playerModel.playedChords // 8) * 50
            ]
            { columns = List.repeat 8 A.fill
            , rows = []
            , cells = chordsGridCells playerModel
            }


chordsGridCells : Model -> List (E.OnGrid (E.Element S.Styles variation Msg))
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


mapChords : S.ActiveInactive -> Int -> Int -> ChordTime -> E.OnGrid (E.Element S.Styles variation Msg)
mapChords activeInactive start idx ( chord, time ) =
    E.cell
        { start = ( (start + idx) % 8, (start + idx) // 8 )
        , width = 1
        , height = 1
        , content =
            E.el (S.ChordItem activeInactive)
                [ A.paddingXY 0 10, Events.onClick <| SeekToPosition time ]
                (E.el S.None
                    [ A.center
                    , A.verticalCenter
                    , A.height <| A.px 20
                    ]
                 <|
                    E.text <|
                        ChordTime.chordName chord
                )
        }



-----------------------------------------------------------------------------------


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



-----------------------------------------------------------------------------------


type alias PropsYouTubeDisplay =
    { pad : Float, ht : Float, wd : Float }


propsYouTubeDisplay : PropsYouTubeDisplay
propsYouTubeDisplay =
    PropsYouTubeDisplay 10 180 320


diplayYouTubeVideo : Model -> PropsYouTubeDisplay -> E.Element S.Styles variation msg
diplayYouTubeVideo { id, playerID } props =
    E.screen <|
        E.el S.YouTubeSpace
            [ A.id <| Player.ytPlayerIDToString playerID
            , A.alignBottom
            , A.padding props.pad
            , A.height <| A.px <| props.ht + props.pad * 2
            , A.width <| A.px <| props.wd + props.pad * 2
            ]
        <|
            ViewSong.displaySongImg id


clearSpaceYouTubeVideo : PropsYouTubeDisplay -> E.Element S.Styles variation msg
clearSpaceYouTubeVideo props =
    E.el S.None
        [ A.padding props.pad
        , A.height <| A.px <| props.ht + props.pad * 2
        , A.width <| A.px <| props.wd + props.pad * 2
        ]
    <|
        E.text ""
