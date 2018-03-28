module App.Model exposing (..)

import App.Ports as P
import Time


type Page
    = Home
    | Play


type alias Model =
    { currPage : Page
    , currSong : Maybe Song
    , playerStatus : PlayerStatus
    , songs : List Song
    , test : String
    }


type alias CurrentSong a =
    { a
        | currSong : Maybe Song
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
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    }


type alias Sequence a =
    { a
        | playedChords : List ChordTime
        , currChord : Maybe ChordTime
        , nextChords : List ChordTime
    }


type alias ChordTime =
    ( Chord, Float )


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
        Nothing
        NotStarted
        [ sampleSong, sampleSong ]
        ""
        ! [ P.sendDataToJS P.ToJSTest ]



--- UPDATE ---


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToSong song ->
            { model | currPage = Play, currSong = Just song, playerStatus = NotStarted } ! []

        ChangePlayerStatus playerStatus ->
            { model | playerStatus = playerStatus } ! []

        OutsideData dataFromJS ->
            case dataFromJS of
                P.FromJSTest ->
                    { model | test = "Data from JS" } ! []

        LogErr err ->
            { model | test = err } ! []

        Tick _ ->
            { model
                | playerStatus = getCurrPlayerStatus model
                , currSong = getCurrSong model
            }
                ! []


getCurrPlayerStatus : CurrentSong a -> PlayerStatus
getCurrPlayerStatus { playerStatus, currSong } =
    case currSong of
        Nothing ->
            NotStarted

        Just song ->
            case song.nextChords of
                [] ->
                    Stopped

                _ ->
                    playerStatus


getCurrSong : CurrentSong a -> Maybe Song
getCurrSong { currSong, playerStatus } =
    case playerStatus of
        Playing ->
            case currSong of
                Nothing ->
                    Nothing

                Just song ->
                    Just <| nextChordSequence song

        _ ->
            currSong


nextChordSequence : Sequence a -> Sequence a
nextChordSequence song =
    let
        getplayed : List ChordTime
        getplayed =
            case song.currChord of
                Nothing ->
                    []

                Just chord ->
                    song.playedChords ++ chord :: []

        ( played, curr, next ) =
            case song.nextChords of
                x :: xs ->
                    ( getplayed, Just x, xs )

                [] ->
                    ( [], Nothing, getplayed )
    in
    { song
        | playedChords = played
        , currChord = curr
        , nextChords = next
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
    Song "PiL5UTTTrxk" "Thalli Pogathey - Video Song | Achcham Yenbadhu Madamaiyada | A R Rahman | STR | Gautham" [] Nothing nextChords


nextChords : List ChordTime
nextChords =
    [ ( ( A, Minor ), 1.0 )
    , ( ( B, Major ), 1.0 )
    , ( ( B, Major ), 1.0 )
    , ( ( D, Minor ), 1.0 )
    , ( ( E, Major ), 1.0 )
    , ( ( F, Major ), 1.0 )
    , ( ( G, Major ), 1.0 )
    , ( ( A, Major ), 1.0 )
    , ( ( B, Major ), 1.0 )
    , ( ( C, Major ), 1.0 )
    , ( ( Cs, Minor ), 1.0 )
    , ( ( Ds, Major ), 1.0 )
    , ( ( Fs, Major ), 1.0 )
    , ( ( Bf, Minor ), 1.0 )
    , ( ( B, Major ), 1.0 )
    , ( ( C, Major ), 1.0 )
    , ( ( D, Major ), 1.0 )
    , ( ( E, Major ), 1.0 )
    , ( ( F, Minor ), 1.0 )
    , ( ( G, Major ), 1.0 )
    ]
