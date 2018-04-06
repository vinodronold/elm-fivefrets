module View.Player exposing (..)

import Data.ChordTime as ChordTime exposing (ChordTime)
import Element as E
import Element.Attributes as A
import Page.Player as Player
import Styles as S
import View.Utils as Utils


view : Player.Model -> E.Element S.Styles variation Player.Msg
view model =
    E.column S.None [] [ displayChords model, displayControl model.playerStatus ]


displayControl : Player.PlayerStatus -> E.Element S.Styles variation Player.Msg
displayControl playerStatus =
    E.row S.None [ A.spacing 10, A.padding 20, A.center ] <| controlButtons playerStatus


controlButtons : Player.PlayerStatus -> List (E.Element S.Styles variation Player.Msg)
controlButtons playerStatus =
    case playerStatus of
        Player.NotStarted ->
            [ Utils.button "Play" (Player.ChangePlayerStatus Player.Playing)
            , Utils.disabledButton "Stop"
            ]

        Player.Playing ->
            [ Utils.button "Pause" (Player.ChangePlayerStatus Player.Paused)
            , Utils.button "Stop" (Player.ChangePlayerStatus Player.Stopped)
            ]

        Player.Paused ->
            [ Utils.button "Play" (Player.ChangePlayerStatus Player.Playing)
            , Utils.button "Stop" (Player.ChangePlayerStatus Player.Stopped)
            ]

        Player.Stopped ->
            [ Utils.button "Play" (Player.ChangePlayerStatus Player.Playing)
            , Utils.disabledButton "Stop"
            ]


displayChords : Player.Model -> E.Element S.Styles variation msg
displayChords playerModel =
    E.grid S.None
        [ A.spacing 10 ]
        { columns = List.repeat 8 A.fill
        , rows = []
        , cells = chordsGridCells playerModel
        }


chordsGridCells : Player.Model -> List (E.OnGrid (E.Element S.Styles variation msg))
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
