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
