module Main exposing (..)

import Element as E
import Html exposing (Html)
import Navigation exposing (Location)
import Page.Errored as Errored
import Page.Home as Home
import Page.Player as Player
import Route exposing (Route)
import Styles as S
import Task exposing (Task)
import View.Master as Master


type Page
    = Blank
    | NotFound
    | Errored Errored.PageLoadError
    | Home Home.Model
    | Player Player.Model


type PageState
    = Loading
    | Loaded Page



--- MODEL ---


type alias Model =
    { pageState : PageState
    }



--- INIT ---


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { pageState = Loading
        }



--- UPDATE ---


type Msg
    = SetRoute (Maybe Route)
    | HomeLoaded (Result Errored.PageLoadError Home.Model)
    | PlayerLoaded (Result Errored.PageLoadError Player.Model)
    | PlayerMsg Player.Msg
    | ChangePlayerStatus Player.PlayerStatus


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
            Master.frame True <| E.el S.None [] (E.text "*** Loading ***")

        Loaded NotFound ->
            Master.frame False <| E.el S.None [] (E.text "*** NotFound ***")

        Loaded Blank ->
            Master.frame False <| E.el S.None [] (E.text "*** Blank ***")

        Loaded (Errored err) ->
            Master.frame False <| Errored.view err

        Loaded (Home homeModel) ->
            Master.frame False <| Home.view homeModel

        Loaded (Player playerModel) ->
            Master.frame False <| E.map PlayerMsg (Player.view playerModel)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Root ->
            model ! [ Route.modifyUrl Route.Home ]

        Just Route.Home ->
            { model | pageState = Loading } ! [ Task.attempt HomeLoaded Home.load ]

        Just (Route.Player youTubeID) ->
            { model | pageState = Loading } ! [ Task.attempt PlayerLoaded <| Player.load youTubeID ]



--- MAIN ---


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
