module Main exposing (..)

import Color exposing (rgba)
import Element as E
import Element.Attributes as A
import Html exposing (Html)
import Style as S
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Style.Transition as Transition


---- MODEL ----


type alias Model =
    { songs : Songs }


type alias Songs =
    List Song


type alias Song =
    { title : String
    , id : String
    , chordSeqence : List Chord
    }


type alias Chord =
    ( ChordName, Float )


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


sampleSong : Song
sampleSong =
    Song "Title 1" "1" [ sampleChordSeq ]


sampleChordSeq : Chord
sampleChordSeq =
    ( ( A, Major ), 1.0 )


init : ( Model, Cmd Msg )
init =
    ( Model [ sampleSong, sampleSong ], Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    E.viewport stylesheet <|
        E.column App
            []
            [ E.row None
                []
                [ appNav
                , appContent model
                ]
            ]


appNav : E.Element Styles variation msg
appNav =
    E.navigationColumn None
        []
        { options =
            [ E.el Logo [ A.padding 50 ] <| E.text "fivefrets"
            , E.column None
                []
                [ E.link "/profile" <| E.el NavItem [ A.padding 10 ] <| E.text "Home"
                , E.link "/profile" <| E.el NavItem [ A.padding 10 ] <| E.text "Profile"
                , E.link "/logout" <| E.el NavItem [ A.padding 10 ] <| E.text "Logout"
                ]
            ]
        , name = "fivefrets"
        }


appContent : Model -> E.Element Styles variation msg
appContent model =
    E.mainContent None [ A.verticalCenter, A.padding 50 ] <| songList model.songs


songList : Songs -> E.Element Styles variation msg
songList songList =
    E.column None [ A.spacing 10 ] <|
        List.map displaySong songList


displaySong : Song -> E.Element Styles variation msg
displaySong song =
    E.row SongItem
        [ A.spacing 20 ]
        [ E.image None [] { src = ytImgUrl song.id, caption = song.title }
        , E.column None
            [ A.spacing 5, A.verticalCenter ]
            [ E.el None [] <| E.text song.title
            ]
        ]


ytImgUrl : String -> String
ytImgUrl id =
    "https://i.ytimg.com/vi/" ++ id ++ "/hqdefault.jpg"



---- STYLE ----


type Styles
    = None
    | App
    | Logo
    | NavItem
    | SongItem


stylesheet : S.StyleSheet Styles variation
stylesheet =
    S.styleSheet
        [ S.style None []
        , S.style App
            [ Font.typeface <| Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } :: []
            , Color.background appColor.background
            , Color.text appColor.primary
            ]
        , S.style Logo
            [ Color.background appColor.primary
            , Color.text appColor.textOnPrimary
            , Font.letterSpacing 5
            , S.prop "font-variant-ligatures" "none"
            , Font.size 16
            ]
        , S.style NavItem
            [ Border.bottom 3
            , Border.solid
            , Color.border appColor.primary
            , S.hover [ Color.background appColor.secondary ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "background" ]
                  }
                ]
            ]
        , S.style SongItem
            [ Shadow.deep
            , S.hover [ Shadow.simple ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "box-shadow" ]
                  }
                ]
            ]
        ]


type alias AppColorPalette =
    { primary : Color.Color
    , lightPrimary : Color.Color
    , textOnPrimary : Color.Color
    , secondary : Color.Color
    , background : Color.Color
    }


primaryRgb : Float -> Color.Color
primaryRgb =
    rgba 38 50 56


appColor : AppColorPalette
appColor =
    { primary = primaryRgb 1
    , lightPrimary = primaryRgb 0.4
    , textOnPrimary = rgba 255 255 255 0.87
    , secondary = rgba 255 212 84 1
    , background = rgba 255 255 255 1
    }



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
