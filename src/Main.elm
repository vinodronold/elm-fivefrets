module Main exposing (..)

-- import Page.Errored exposing (PageLoadError)

import Element as E
import Element.Attributes as A
import Element.Events as Event
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Navigation exposing (Location)
import Styles as S
import Task exposing (Task)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home HomeModel
    | Player PlayerModel


type PageLoadError
    = PageLoadError String


type PageState
    = Loading
    | Loaded Page



--- MODEL ---


type alias Model =
    { pageState : PageState
    }


type alias HomeModel =
    { songs : Songs
    }


type alias PlayerModel =
    { id : SongID
    , playerStatus : PlayerStatus
    , playedChords : List ChordTime
    , currChord : Maybe ChordTime
    , nextChords : List ChordTime
    }


type PlayerStatus
    = NotStarted
    | Playing
    | Paused
    | Stopped


type alias Songs =
    List SongID


type alias SongID =
    { id : YouTubeID
    , title : String
    , imgUrl : String
    }


type YouTubeID
    = YouTubeID String


youTubeIDtoString : YouTubeID -> String
youTubeIDtoString (YouTubeID id) =
    id


youTubeIDParser : Parser (YouTubeID -> a) a
youTubeIDParser =
    Url.custom "SONG" (Ok << YouTubeID)


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



--- INIT ---


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (fromLocation location)
        { pageState = Loading
        }



--- UPDATE ---


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result PageLoadError HomeModel)
    | PlayerLoaded (Result PageLoadError PlayerModel)
    | ChangePlayerStatus PlayerStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute route ->
            setRoute route model

        HomeLoaded (Ok homeModel) ->
            { model | pageState = Loaded (Home homeModel) } ! []

        HomeLoaded (Err errMessage) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        PlayerLoaded (Ok playerModel) ->
            { model | pageState = Loaded (Player playerModel) } ! []

        PlayerLoaded (Err errMessage) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        _ ->
            model ! []



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--- VIEW ---


view : Model -> Html Msg
view model =
    case model.pageState of
        Loading ->
            frame True <| E.el S.None [] (E.text "*** Loading ***")

        Loaded NotFound ->
            frame False <| E.el S.None [] (E.text "*** NotFound ***")

        Loaded Blank ->
            frame False <| E.el S.None [] (E.text "*** Blank ***")

        Loaded (Errored (PageLoadError errMessage)) ->
            frame False <| E.el S.None [] (E.text (errMessage ++ "*** Errored ***"))

        Loaded (Home homeModel) ->
            frame False <| viewHome homeModel

        Loaded (Player playerModel) ->
            frame False <| viewPlayer playerModel



--- HOME PAGE ---


viewHome : HomeModel -> E.Element S.Styles variation Msg
viewHome { songs } =
    E.column S.None [ A.spacing 10 ] <|
        List.map displaySong songs


displaySong : SongID -> E.Element S.Styles variation Msg
displaySong songID =
    E.link (href <| PlayerRoute songID.id) <|
        E.row S.SongItem
            [ A.spacing 20 ]
            [ E.image S.None [] { src = songID.imgUrl, caption = songID.title }
            , E.column S.None
                [ A.spacing 5, A.verticalCenter ]
                [ E.el S.None [] <| E.text songID.title
                ]
            ]



--- PLAYER ---


viewPlayer : PlayerModel -> E.Element S.Styles variation Msg
viewPlayer model =
    E.column S.None [] [ displayChords model, displayControl model.playerStatus ]


displayControl : PlayerStatus -> E.Element S.Styles variation Msg
displayControl playerStatus =
    E.row S.None [ A.spacing 10, A.padding 20, A.center ] <| controlButtons playerStatus


controlButtons : PlayerStatus -> List (E.Element S.Styles variation Msg)
controlButtons playerStatus =
    case playerStatus of
        NotStarted ->
            [ button "Play" (ChangePlayerStatus Playing)
            , disabledButton "Stop"
            ]

        Playing ->
            [ button "Pause" (ChangePlayerStatus Paused)
            , button "Stop" (ChangePlayerStatus Stopped)
            ]

        Paused ->
            [ button "Play" (ChangePlayerStatus Playing)
            , button "Stop" (ChangePlayerStatus Stopped)
            ]

        Stopped ->
            [ button "Play" (ChangePlayerStatus Playing)
            , disabledButton "Stop"
            ]


displayChords : PlayerModel -> E.Element S.Styles variation msg
displayChords playerModel =
    E.grid S.None
        [ A.spacing 10 ]
        { columns = List.repeat 8 A.fill
        , rows = []
        , cells = chordsGridCells playerModel
        }


chordsGridCells : PlayerModel -> List (E.OnGrid (E.Element S.Styles variation msg))
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
                (E.text <| chordName chord)
        }


chordName : Chord -> String
chordName ( note, quality ) =
    noteToString note ++ qualityToString quality


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


qualityToString : Quality -> String
qualityToString q =
    case q of
        Major ->
            ""

        Minor ->
            "m"



--- VIEW UTLIS FN ---


disabledButton : String -> E.Element S.Styles variation msg
disabledButton label =
    E.el (S.Button S.Inactive) [ A.paddingXY 10 5 ] <| E.text label


button : String -> Msg -> E.Element S.Styles variation Msg
button label msg =
    E.button (S.Button S.Active) [ A.paddingXY 10 5, Event.onClick msg ] <| E.text label



--- MASTER VIEW ---


frame : Bool -> E.Element S.Styles variation msg -> Html msg
frame isLoading pageContent =
    E.viewport S.stylesheet <|
        E.column S.App
            []
            [ E.row S.None
                []
                [ appNav
                , E.column S.None
                    [ A.width A.fill ]
                    [ topBar isLoading "Home Page"
                    , E.mainContent S.None [ A.verticalCenter, A.padding 50 ] <| pageContent
                    ]
                ]
            ]


appNav : E.Element S.Styles variation msg
appNav =
    E.navigationColumn S.None
        []
        { options =
            [ E.el S.Logo [ A.padding 50 ] <| E.text "fivefrets"
            , E.column S.None
                []
                [ E.link "/" <| E.el S.NavItem [ A.padding 10 ] <| E.text "Home"
                , E.link "/profile" <| E.el S.NavItem [ A.padding 10 ] <| E.text "Profile"
                , E.link "/logout" <| E.el S.NavItem [ A.padding 10 ] <| E.text "Logout"
                ]
            ]
        , name = "fivefrets"
        }


topBar : Bool -> String -> E.Element S.Styles variation msg
topBar isLoading title =
    let
        topContent =
            if isLoading then
                "Loading  . . ."
            else
                title
    in
    E.row S.TopBar
        [ A.padding 10, A.spacing 10 ]
        [ E.el S.None [ A.alignLeft ] <| E.text "X"
        , E.el S.None [ A.spread ] <| E.text (Debug.log "topContent" topContent)
        ]



--- TASKS ---


loadHomePage : Task PageLoadError HomeModel
loadHomePage =
    let
        loadSongs =
            songsFeed
                |> Http.toTask

        handleLoadError _ =
            PageLoadError "Homepage is currently unavailable."
    in
    Task.map HomeModel loadSongs
        |> Task.mapError handleLoadError


loadPlayerPage : YouTubeID -> Task PageLoadError PlayerModel
loadPlayerPage youTubeID =
    -- let
    --     loadPlayer =
    --         playerData
    --             |> Http.toTask
    --     handleLoadError _ =
    --         PageLoadError "Chords Player is currently unavailable."
    -- in
    Task.succeed <|
        PlayerModel { id = youTubeID, title = "PLAYER SONG", imgUrl = "" }
            NotStarted
            []
            Nothing
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



--- REQUESTS ---


songsFeed : Http.Request (List SongID)
songsFeed =
    let
        searchText =
            "Beatles"

        ytApiKey =
            "AIzaSyDt03O45GRK2doERZICfzCgUbeXVFtLpiY"

        ytApiUrl =
            "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=5&type=video&q=" ++ searchText ++ "&key=" ++ ytApiKey

        request =
            Http.get ytApiUrl decodeYTUrl
    in
    request


decodeYTUrl : Decode.Decoder (List SongID)
decodeYTUrl =
    decodeYTItem
        |> Decode.list
        |> Decode.field "items"


decodeYTItem : Decode.Decoder SongID
decodeYTItem =
    Decode.map3 SongID
        (Decode.at [ "id", "videoId" ] decodeYouTubeID)
        (Decode.at [ "snippet", "title" ] Decode.string)
        (Decode.at [ "snippet", "thumbnails", "default", "url" ] Decode.string)


decodeYouTubeID : Decode.Decoder YouTubeID
decodeYouTubeID =
    Decode.map YouTubeID Decode.string



--- ROUTER ---


type Route
    = HomeRoute
    | Root
    | PlayerRoute YouTubeID


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map HomeRoute (s "")
        , Url.map PlayerRoute (s "player" </> youTubeIDParser)
        ]


href : Route -> String
href page =
    let
        pieces =
            case page of
                HomeRoute ->
                    []

                Root ->
                    []

                PlayerRoute youTubeID ->
                    [ "player", youTubeIDtoString youTubeID ]
    in
    "#/" ++ String.join "/" pieces


modifyUrl : Route -> Cmd msg
modifyUrl =
    href >> Navigation.modifyUrl


newUrl : Route -> Cmd msg
newUrl =
    href >> Navigation.newUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        parseHash route location


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Root ->
            model ! [ modifyUrl HomeRoute ]

        Just HomeRoute ->
            { model | pageState = Loading } ! [ Task.attempt HomeLoaded loadHomePage ]

        Just (PlayerRoute youTubeID) ->
            { model | pageState = Loading } ! [ Task.attempt PlayerLoaded <| loadPlayerPage youTubeID ]



--- MAIN ---


main : Program Never Model Msg
main =
    Navigation.program (fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
