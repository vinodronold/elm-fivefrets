module App.Model exposing (..)

import App.Ports as P
import Time


type alias Model =
    { currPage : Page
    , currSong : CurrentSong
    , songs : List Song
    , test : String
    }


type Page
    = Home
    | Play


type alias CurrentSong =
    { song : Maybe Song
    , playerStatus : PlayerStatus
    }


type PlayerStatus
    = NotStarted
    | Playing
    | Paused
    | Stopped


type alias Song =
    { id : String
    , title : String
    , sequence : Sequence
    }


type alias Sequence =
    { prev : List ChordTime
    , curr : Maybe ChordTime
    , next : List ChordTime
    }


type alias ChordTime =
    { id : Int
    , chord : Chord
    , time : Float
    }


type alias Chord =
    ( Note, Quality )


type Note
    = A
    | As
    | Bf
    | B
    | C
    | Cs
    | Df
    | D
    | Ds
    | Ef
    | E
    | F
    | Fs
    | Gf
    | G
    | Gs
    | Af


type Quality
    = Major
    | Minor


type Msg
    = GoToSong Song
    | ChangePlayerStatus PlayerStatus
    | OutsideData P.DataFromJS
    | LogErr String
    | Tick Time.Time



--- INIT ---


init : ( Model, Cmd Msg )
init =
    Model Home
        (CurrentSong Nothing NotStarted)
        [ sampleSong, sampleSong ]
        ""
        ! [ P.sendDataToJS P.ToJSTest ]



--- UPDATE ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToSong song ->
            { model | currPage = Play, currSong = CurrentSong (Just song) NotStarted } ! []

        ChangePlayerStatus playerStatus ->
            { model | currSong = CurrentSong model.currSong.song playerStatus } ! []

        OutsideData dataFromJS ->
            case dataFromJS of
                P.FromJSTest ->
                    { model | test = "Data from JS" } ! []

        LogErr err ->
            { model | test = err } ! []

        Tick _ ->
            let
                currSong =
                    case model.currSong.playerStatus of
                        Playing ->
                            case model.currSong.song of
                                Nothing ->
                                    Nothing

                                Just song ->
                                    Just
                                        (Song song.id
                                            song.title
                                            (nextChordSequence song.sequence)
                                        )

                        _ ->
                            model.currSong.song
            in
            { model | currSong = CurrentSong currSong model.currSong.playerStatus } ! []


nextChordSequence : Sequence -> Sequence
nextChordSequence chordSeq =
    let
        getprev =
            case chordSeq.curr of
                Nothing ->
                    []

                Just chord ->
                    chordSeq.prev ++ chord :: []

        ( prev, curr, next ) =
            case chordSeq.next of
                x :: xs ->
                    ( getprev, Just x, xs )

                [] ->
                    ( [], Nothing, getprev )
    in
    { prev = prev
    , curr = curr
    , next = next
    }



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ P.getDataFromJS OutsideData LogErr
        , subTimer model
        ]


subTimer : Model -> Sub Msg
subTimer model =
    case model.currPage of
        Home ->
            Sub.none

        Play ->
            Time.every (1 * Time.second) Tick



-- case model.currSong.sequence.next of
--     [] ->
--         Sub.none
--     _ :: [] ->
--         Time.every (1 * Time.second) Tick
--- SAMPLE MODEL ---


sampleSong : Song
sampleSong =
    Song "Title 1" "1" sampleChordSeq


sampleChordSeq : Sequence
sampleChordSeq =
    { prev = []
    , curr = Nothing
    , next =
        [ ChordTime 1 ( A, Minor ) 1.0
        , ChordTime 2 ( B, Major ) 1.0
        , ChordTime 3 ( B, Major ) 1.0
        , ChordTime 4 ( D, Minor ) 1.0
        , ChordTime 5 ( E, Major ) 1.0
        , ChordTime 6 ( F, Major ) 1.0
        , ChordTime 7 ( G, Major ) 1.0
        , ChordTime 8 ( A, Major ) 1.0
        , ChordTime 9 ( B, Major ) 1.0
        , ChordTime 10 ( C, Major ) 1.0
        , ChordTime 11 ( Cs, Minor ) 1.0
        , ChordTime 12 ( Ds, Major ) 1.0
        , ChordTime 13 ( Fs, Major ) 1.0
        , ChordTime 14 ( Bf, Minor ) 1.0
        , ChordTime 15 ( B, Major ) 1.0
        , ChordTime 16 ( C, Major ) 1.0
        , ChordTime 17 ( D, Major ) 1.0
        , ChordTime 18 ( E, Major ) 1.0
        , ChordTime 19 ( F, Minor ) 1.0
        , ChordTime 20 ( G, Major ) 1.0
        ]
    }
