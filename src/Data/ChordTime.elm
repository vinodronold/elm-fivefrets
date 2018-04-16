module Data.ChordTime exposing (ChordTime, chordName, sample)

-----------------------------------------------------------------------


type alias ChordTime =
    ( Chord, Float )



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
    ]
