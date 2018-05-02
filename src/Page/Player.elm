module Page.Player exposing (Model, Msg(..), load, update, view)

import Data.ChordTime as ChordTime exposing (ChordTime)
import Data.Player as Player exposing (PlayerStatus, YTPlayerID)
import Data.Song as Song exposing (SongID, YouTubeID)
import Dom
import Dom.Scroll as Scroll
import Element as E
import Element.Attributes as A
import Element.Events as Events
import Http
import Maybe
import Page.Errored as Errored
import Ports
import Styles as S
import Task exposing (Task)
import Time exposing (Time)
import View.Songs as ViewSong
import View.Utils as Utils
import Window


type alias Model =
    { playerID : YTPlayerID
    , playerStatus : PlayerStatus
    , playerTime : Maybe Time
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    , device : E.Device
    , id : SongID
    }


load : YouTubeID -> Task Errored.PageLoadError Model
load youTubeID =
    let
        initModel =
            Model (Player.YTPlayerID "YT_Player")
                Player.NotLoaded
                Nothing
                []
                Nothing
                ChordTime.sample

        getSongID =
            youTubeID
                |> Player.videoDetails
                |> Http.toTask

        windowSize =
            Task.map E.classifyDevice Window.size

        handleLoadError e =
            Errored.pageLoadError "Chords Player is currently unavailable."

        -- Errored.pageLoadError (Debug.log "request" (toString e))
    in
    Task.map2 initModel
        windowSize
        getSongID
        |> Task.mapError handleLoadError


type Msg
    = Load
    | ChangePlayerStatus PlayerStatus
    | UpdatePlayerStatus PlayerStatus
    | UpdatePlayerTime Time
    | SeekToPosition Time
    | Tick Time
    | ScrollingToY
    | WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            model ! [ Ports.pushDataToJS Ports.GetPlayerCurrTime ]

        Load ->
            model ! [ Ports.pushDataToJS <| Ports.LoadYouTubeVideo model.playerID model.id.ytid ]

        ChangePlayerStatus playerStatus ->
            if playerStatus == Player.Ended then
                { model
                    | playedChords = []
                    , currChord = Nothing
                    , nextChords = getAllChords model
                }
                    ! [ Ports.pushDataToJS <| Ports.SetPlayerState playerStatus, scrollToTop diplayChordDomID ]
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
            in
            if playerTime > ChordTime.getTime model.currChord then
                case model.nextChords of
                    x :: xs ->
                        { model | playerTime = Just playerTime, playedChords = getplayed, currChord = Just x, nextChords = xs }
                            ! [ scrolling (List.length model.playedChords + 1) (getBlocks model.device) ]

                    [] ->
                        -- Chords ENDED
                        { model | playerTime = Just playerTime, playedChords = [], currChord = Nothing, nextChords = getplayed }
                            ! [ Ports.pushDataToJS <| Ports.SetPlayerState Player.Ended, scrollToTop diplayChordDomID ]
            else
                { model | playerTime = Just playerTime }
                    ! []

        SeekToPosition seekToTime ->
            let
                ( before, after ) =
                    getAllChords model
                        |> List.partition
                            (\( _, t ) -> t < seekToTime)

                ( curr, next ) =
                    case after of
                        x :: xs ->
                            ( Just x, xs )

                        [] ->
                            ( Nothing, [] )
            in
            { model
                | playedChords = before
                , currChord = curr
                , nextChords = next
            }
                ! [ Ports.pushDataToJS <| Ports.SeekTo seekToTime
                  , scrollToY (getScrollYPos (List.length before) (getBlocks model.device)) diplayChordDomID
                  ]

        ScrollingToY ->
            model ! []

        WindowResize size ->
            { model | device = E.classifyDevice size } ! []


scrolling : Int -> Int -> Cmd Msg
scrolling totalChordsPlayed blocks =
    if totalChordsPlayed > blocks then
        if totalChordsPlayed % blocks == 0 then
            scrollToY (getScrollYPos totalChordsPlayed blocks) diplayChordDomID
        else
            Cmd.none
    else
        Cmd.none


view : Model -> E.Element S.Styles variation Msg
view model =
    E.column S.None
        []
        [ displayTitle model
        , displayChords model
        , displayControl model.playerStatus
        , clearSpaceYouTubeVideo propsYouTubeDisplay
        , diplayYouTubeVideo model propsYouTubeDisplay
        ]



-----------------------------------------------------------------------------------


type alias Title r =
    { r | id : SongID }


displayTitle : Title r -> E.Element S.Styles variation msg
displayTitle { id } =
    E.h1 S.Title [ A.paddingXY 10 20 ] <| E.paragraph S.None [] [ E.text id.title ]



-----------------------------------------------------------------------------------


type alias Chords r =
    { r
        | playedChords : List ChordTime
        , currChord : Maybe ChordTime
        , nextChords : List ChordTime
        , device : E.Device
    }


getAllChords : Chords r -> List ChordTime
getAllChords { playedChords, currChord, nextChords } =
    case currChord of
        Nothing ->
            nextChords

        Just curr ->
            playedChords ++ (curr :: nextChords)


displayChords : Chords r -> E.Element S.Styles variation Msg
displayChords chords =
    E.grid S.ChordGridContainer
        [ A.spacing 10
        , A.padding 10
        ]
        { columns = List.repeat (getBlocks chords.device) A.fill
        , rows = []
        , cells = chordsGridCells chords
        }
        |> E.el S.None
            [ A.height <| A.px 150
            , A.clip
            , A.id diplayChordDomID
            , A.inlineStyle [ ( "scroll-behavior", "smooth" ) ]
            ]


chordsGridCells : Chords r -> List (E.OnGrid (E.Element S.Styles variation Msg))
chordsGridCells { playedChords, currChord, nextChords, device } =
    let
        played =
            List.indexedMap (mapChords device S.Inactive 0) playedChords

        ( nextStart, curr ) =
            case currChord of
                Nothing ->
                    ( List.length playedChords
                    , []
                    )

                Just chord ->
                    ( List.length playedChords + 1
                    , List.indexedMap (mapChords device S.Active <| List.length playedChords) <| chord :: []
                    )

        next =
            List.indexedMap (mapChords device S.Inactive nextStart) nextChords
    in
    played ++ curr ++ next


mapChords : E.Device -> S.ActiveInactive -> Int -> Int -> ChordTime -> E.OnGrid (E.Element S.Styles variation Msg)
mapChords device activeInactive start idx ( chord, time ) =
    E.cell
        { start = ( (start + idx) % getBlocks device, (start + idx) // getBlocks device )
        , width = 1
        , height = 1
        , content =
            E.el (S.ChordItem activeInactive)
                [ A.paddingXY 0 10, Events.onClick <| SeekToPosition time ]
                (ChordTime.chordName chord
                    |> E.text
                    |> E.el S.None
                        [ A.center
                        , A.verticalCenter
                        , A.height <| A.px 20
                        ]
                )
        }



-----------------------------------------------------------------------------------


displayControl : PlayerStatus -> E.Element S.Styles variation Msg
displayControl playerStatus =
    E.row S.None [ A.spacing 10, A.padding 20, A.center ] <| controlButtons playerStatus


controlButtons : PlayerStatus -> List (E.Element S.Styles variation Msg)
controlButtons playerStatus =
    case playerStatus of
        Player.NotLoaded ->
            [ Utils.button "Play" Load
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


type alias YouTubeVideo r =
    { r | id : SongID, playerID : YTPlayerID }


type alias PropsYouTubeDisplay =
    { pad : Float, ht : Float, wd : Float }


propsYouTubeDisplay : PropsYouTubeDisplay
propsYouTubeDisplay =
    PropsYouTubeDisplay 10 180 320


diplayYouTubeVideo : YouTubeVideo r -> PropsYouTubeDisplay -> E.Element S.Styles variation msg
diplayYouTubeVideo { id, playerID } props =
    ViewSong.displaySongImg id
        |> E.el S.YouTubeSpace
            [ A.id <| Player.ytPlayerIDToString playerID
            , A.alignBottom
            , A.padding props.pad
            , A.maxHeight <| A.px <| props.ht + props.pad * 2
            , A.maxWidth <| A.px <| props.wd + props.pad * 2
            ]
        |> E.screen


clearSpaceYouTubeVideo : PropsYouTubeDisplay -> E.Element S.Styles variation msg
clearSpaceYouTubeVideo props =
    E.text ""
        |> E.el S.None
            [ A.padding props.pad
            , A.height <| A.px <| props.ht + props.pad * 2
            , A.width <| A.px <| props.wd + props.pad * 2
            ]



---------------------------------------------------------------------------------------------------
-- UTILITIES
---------------------------------------------------------------------------------------------------


diplayChordDomID : Dom.Id
diplayChordDomID =
    "diplayChordID"


getBlocks : E.Device -> Int
getBlocks { tablet, phone } =
    if phone || tablet then
        4
    else
        8



---> SCROLLING


getScrollYPos : Int -> Int -> Float
getScrollYPos idx blocks =
    toFloat ((idx // blocks) - 1) * 55


scrollToTop : Dom.Id -> Cmd Msg
scrollToTop =
    scrollToY 0


scrollToY : Float -> Dom.Id -> Cmd Msg
scrollToY pos domID =
    Scroll.toY domID pos
        |> Task.attempt (always ScrollingToY)



---------------------------------------------------------------------------------------------------
