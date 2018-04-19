module Data.ChordTime exposing (ChordTime, chordName, getTime, sample)

import Time exposing (Time)


-----------------------------------------------------------------------


type alias ChordTime =
    ( Chord, Time )


getTime : Maybe ChordTime -> Time
getTime chordTime =
    case chordTime of
        Nothing ->
            0

        Just ( _, t ) ->
            t



-----------------------------------------------------------------------


type alias Chord =
    ( Note, Quality )


chordName : Chord -> String
chordName ( note, quality ) =
    noteToString note ++ qualityToString quality



-----------------------------------------------------------------------


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


noteToString : Note -> String
noteToString note =
    case note of
        A ->
            "A"

        As ->
            "A#"

        Bf ->
            "Bb"

        B ->
            "B"

        C ->
            "C"

        Cs ->
            "C#"

        Df ->
            "Db"

        D ->
            "D"

        Ds ->
            "D#"

        Ef ->
            "Eb"

        E ->
            "E"

        F ->
            "F"

        Fs ->
            "F#"

        Gf ->
            "Gb"

        G ->
            "G"

        Gs ->
            "G#"

        Af ->
            "Ab"



-----------------------------------------------------------------------


type Quality
    = Major
    | Minor


qualityToString : Quality -> String
qualityToString q =
    case q of
        Major ->
            ""

        Minor ->
            "m"



-----------------------------------------------------------------------


sample : List ChordTime
sample =
    [ ( ( A, Minor ), 1.0 )
    , ( ( B, Major ), 2.0 )
    , ( ( B, Major ), 3.0 )
    , ( ( D, Minor ), 4.0 )
    , ( ( E, Major ), 5.0 )
    , ( ( F, Major ), 6.0 )
    , ( ( G, Major ), 7.0 )
    , ( ( A, Major ), 8.0 )
    , ( ( B, Major ), 9.0 )
    , ( ( C, Major ), 10.0 )
    , ( ( Cs, Minor ), 11.0 )
    , ( ( Ds, Major ), 12.0 )
    , ( ( Fs, Major ), 13.0 )
    , ( ( Bf, Minor ), 14.0 )
    , ( ( B, Major ), 15.0 )
    , ( ( C, Major ), 16.0 )
    , ( ( D, Major ), 17.0 )
    , ( ( E, Major ), 18.0 )
    , ( ( F, Minor ), 19.0 )
    , ( ( G, Major ), 20.0 )
    , ( ( A, Minor ), 21.0 )
    , ( ( B, Major ), 22.0 )
    , ( ( B, Major ), 23.0 )
    , ( ( D, Minor ), 24.0 )
    , ( ( E, Major ), 25.0 )
    , ( ( F, Major ), 26.0 )
    , ( ( G, Major ), 27.0 )
    , ( ( A, Major ), 28.0 )
    , ( ( B, Major ), 29.0 )
    , ( ( C, Major ), 30.0 )
    , ( ( Cs, Minor ), 31.0 )
    , ( ( Ds, Major ), 32.0 )
    , ( ( Fs, Major ), 33.0 )
    , ( ( Bf, Minor ), 34.0 )
    , ( ( B, Major ), 35.0 )
    , ( ( C, Major ), 36.0 )
    , ( ( D, Major ), 37.0 )
    , ( ( E, Major ), 38.0 )
    , ( ( F, Minor ), 39.0 )
    , ( ( G, Major ), 40.0 )
    ]
