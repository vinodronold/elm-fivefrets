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
    Maybe Song


type alias Song =
    { id : String
    , title : String
    , chordSeqence : ChordSequence
    }


type alias ChordSequence =
    { prev : List Chord
    , curr : Maybe Chord
    , next : List Chord
    }


type alias Chord =
    { id : Int
    , name : ChordName
    , time : Float
    }


type alias ChordName =
    ( ChordType, ChordQuality )


type ChordType
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type ChordQuality
    = Major
    | Minor


init : ( Model, Cmd Msg )
init =
    Model Home Nothing [ sampleSong, sampleSong ] "" ! [ P.sendDataToJS P.ToJSTest ]



---- UPDATE ----


type Msg
    = GoToSong Song
    | OutsideData P.DataFromJS
    | LogErr String
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToSong song ->
            { model | currPage = Play, currSong = Just song } ! []

        OutsideData dataFromJS ->
            case dataFromJS of
                P.FromJSTest ->
                    { model | test = "Data from JS" } ! []

        LogErr err ->
            { model | test = err } ! []

        Tick _ ->
            let
                currSong =
                    case model.currSong of
                        Nothing ->
                            Nothing

                        Just song ->
                            Just
                                (Song song.id
                                    song.title
                                    (nextChordSequence song.chordSeqence)
                                )
            in
            { model | currSong = currSong } ! []


nextChordSequence : ChordSequence -> ChordSequence
nextChordSequence chordSeq =
    let
        getprev =
            case chordSeq.curr of
                Nothing ->
                    []

                Just chord ->
                    chord :: chordSeq.prev

        ( prev, curr, next ) =
            case chordSeq.next of
                x :: xs ->
                    ( getprev, Just x, xs )

                [] ->
                    ( [], Nothing, List.reverse getprev )
    in
    { prev = prev
    , curr = curr
    , next = next
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ P.getDataFromJS OutsideData LogErr
        , Time.every (1 * Time.second) Tick
        ]


sampleSong : Song
sampleSong =
    Song "Title 1" "1" sampleChordSeq


sampleChordSeq : ChordSequence
sampleChordSeq =
    { prev = []
    , curr = Nothing
    , next =
        [ Chord 1 ( A, Major ) 1.0
        , Chord 2 ( A, Major ) 1.0
        , Chord 3 ( A, Major ) 1.0
        , Chord 4 ( A, Major ) 1.0
        , Chord 5 ( A, Major ) 1.0
        , Chord 6 ( A, Major ) 1.0
        , Chord 7 ( A, Major ) 1.0
        , Chord 8 ( A, Major ) 1.0
        , Chord 9 ( A, Major ) 1.0
        , Chord 10 ( A, Major ) 1.0
        , Chord 11 ( A, Major ) 1.0
        , Chord 12 ( A, Major ) 1.0
        , Chord 13 ( A, Major ) 1.0
        , Chord 14 ( A, Major ) 1.0
        , Chord 15 ( A, Major ) 1.0
        , Chord 16 ( A, Major ) 1.0
        , Chord 17 ( A, Major ) 1.0
        , Chord 18 ( A, Major ) 1.0
        , Chord 19 ( A, Major ) 1.0
        , Chord 20 ( A, Major ) 1.0
        ]
    }
