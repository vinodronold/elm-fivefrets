module App.View exposing (view)

import App.Model as M
import App.Styles as S
import Element as E
import Element.Attributes as A
import Element.Events as Event
import Html exposing (Html)


view : M.Model -> Html M.Msg
view model =
    E.viewport S.stylesheet <|
        E.column S.App
            []
            [ E.row S.None
                []
                [ appNav
                , appContent model
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


appContent : M.Model -> E.Element S.Styles variation M.Msg
appContent model =
    E.column S.None
        [ A.width A.fill ]
        [ topBar "Home Page"
        , E.mainContent S.None [ A.verticalCenter, A.padding 50 ] <| appPage model
        ]


appPage : M.Model -> E.Element S.Styles variation M.Msg
appPage model =
    case model.currPage of
        M.Home ->
            songList model.songs

        M.Play ->
            playSong model.currSong


topBar : String -> E.Element S.Styles variation M.Msg
topBar title =
    E.row S.TopBar
        [ A.padding 10, A.spacing 10 ]
        [ E.el S.None [ A.alignLeft ] <| E.text "X"
        , E.el S.None [ A.spread ] <| E.text title
        ]


songList : List M.Song -> E.Element S.Styles variation M.Msg
songList songList =
    E.column S.None [ A.spacing 10 ] <|
        List.map displaySong songList


displaySong : M.Song -> E.Element S.Styles variation M.Msg
displaySong song =
    E.row S.SongItem
        [ A.spacing 20, Event.onClick <| M.GoToSong song ]
        [ E.image S.None [] { src = ytImgUrl song.id, caption = song.title }
        , E.column S.None
            [ A.spacing 5, A.verticalCenter ]
            [ E.el S.None [] <| E.text song.title
            ]
        ]


ytImgUrl : String -> String
ytImgUrl id =
    "https://i.ytimg.com/vi/" ++ id ++ "/hqdefault.jpg"


playSong : M.CurrentSong -> E.Element S.Styles variation msg
playSong currSong =
    case currSong of
        Nothing ->
            E.el S.None [] <| E.text "No Songs Selected. Please go back to select a song"

        Just song ->
            displayChords song


displayChords : M.Song -> E.Element S.Styles variation msg
displayChords song =
    E.grid S.None
        [ A.spacing 10 ]
        { columns = List.repeat 8 A.fill
        , rows = []
        , cells = chordsGridCells song.chordSeqence
        }


chordsGridCells : M.ChordSequence -> List (E.OnGrid (E.Element S.Styles variation msg))
chordsGridCells chordSequence =
    let
        prevChords =
            List.indexedMap (mapChords S.Inactive 0) chordSequence.prev

        ( nextChordStart, currChord ) =
            case chordSequence.curr of
                Nothing ->
                    ( List.length chordSequence.prev
                    , []
                    )

                Just chord ->
                    ( List.length chordSequence.prev + 1
                    , List.indexedMap (mapChords S.Active <| List.length chordSequence.prev) <| chord :: []
                    )

        nextChords =
            List.indexedMap (mapChords S.Inactive nextChordStart) chordSequence.next
    in
    prevChords ++ currChord ++ nextChords


mapChords : S.ActiveInactive -> Int -> Int -> M.Chord -> E.OnGrid (E.Element S.Styles variation msg)
mapChords activeInactive start idx chord =
    E.cell
        { start = ( (start + idx) % 8, (start + idx) // 8 )
        , width = 1
        , height = 1
        , content =
            E.el (S.ChordItem activeInactive)
                [ A.paddingXY 0 10
                , A.inlineStyle [ ( "text-align", "center" ) ]
                ]
                (E.text <| "A" ++ (toString <| start + idx))
        }



-- chordItemStyle : M.ChordPosition -> Int -> S.Styles
-- chordItemStyle chordPosition idx =
--     if chordPosition == idx + 1 then
--         S.ChordItem S.Active
--     else
--         S.ChordItem S.Inactive
