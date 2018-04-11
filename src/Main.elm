port module Main exposing (..)

import Element as E
import Html exposing (Html)
import Navigation exposing (Location)
import Page.Errored as Errored
import Page.Home as Home
import Page.Player as Player
import Ports
import Route exposing (Route)
import Styles as S
import Task exposing (Task)
import Time exposing (Time)
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
    | PortMsg Ports.JSDataIn
    | PortErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( PortMsg elmData, _ ) ->
            model ! []

        ( PortErr err, _ ) ->
            model ! []

        ( HomeLoaded (Ok homeModel), _ ) ->
            { model | pageState = Loaded (Home homeModel) } ! []

        ( HomeLoaded (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        ( PlayerLoaded (Ok playerModel), _ ) ->
            { model | pageState = Loaded (Player playerModel) }
                ! [ Ports.pushDataToJS <| Ports.LoadYouTubeVideo playerModel.id.ytid ]

        ( PlayerLoaded (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        ( PlayerMsg playerMsg, Player playerModel ) ->
            { model | pageState = Loaded (Player (Player.update playerMsg playerModel)) } ! []

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model ! []

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model ! []



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions <| getPage model.pageState
        , Ports.pullJSDataToElm PortMsg PortErr
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Player playerModel ->
            if playerModel.playerStatus == Player.Playing then
                Sub.map PlayerMsg <| Time.every Time.second Player.Tick
            else
                Sub.none

        _ ->
            Sub.none


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        Loading ->
            Blank



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
