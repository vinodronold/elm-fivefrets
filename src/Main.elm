port module Main exposing (..)

import Data.Player as PlayerData
import Element as E
import Html exposing (Html)
import Navigation exposing (Location)
import Page.Errored as Errored
import Page.Home as Home
import Page.Loading as PageLoading
import Page.Player as Player
import Ports
import Route exposing (Route)
import Styles as S
import Task exposing (Task)
import Time exposing (Time)
import View.Master as Master
import Window


type Page
    = Blank
    | NotFound
    | Errored Errored.PageLoadError
    | Home Home.Model
    | Player Player.Model


type PageState
    = Loading PageLoading.Model
    | Loaded Page



--- MODEL ---


type alias Model =
    { navOpen : Bool
    , pageState : PageState
    }



--- INIT ---


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)
        { navOpen = False
        , pageState = Loading PageLoading.getInitModel
        }



--- UPDATE ---


type Msg
    = SetRoute (Maybe Route)
    | ToggleMenu
    | HomeLoaded (Result Errored.PageLoadError Home.Model)
    | PlayerLoaded (Result Errored.PageLoadError Player.Model)
    | PlayerMsg Player.Msg
    | PageLoadingMsg PageLoading.Msg
    | PortMsg Ports.JSDataIn
    | PortErr String
    | WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.pageState msg model



-- updatePage (getPage model.pageState) msg model


updatePage : PageState -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( ToggleMenu, _ ) ->
            { model | navOpen = not model.navOpen } ! []

        ( PortErr err, _ ) ->
            model ! []

        ( HomeLoaded (Ok homeModel), _ ) ->
            { model | pageState = Loaded (Home homeModel) } ! []

        ( HomeLoaded (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        ( PlayerLoaded (Ok playerModel), _ ) ->
            { model | pageState = Loaded (Player playerModel) } ! []

        ( PlayerLoaded (Err errMessage), _ ) ->
            { model | pageState = Loaded (Errored errMessage) } ! []

        ( WindowResize size, Loaded (Player playerModel) ) ->
            let
                ( subModel, subCmd ) =
                    Player.update
                        (Player.WindowResize size)
                        playerModel
            in
            { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

        ( PortMsg jsDataIn, Loaded (Player playerModel) ) ->
            case jsDataIn of
                Ports.JSPlayerStatus jsPlayerStatus ->
                    let
                        ( subModel, subCmd ) =
                            Player.update
                                (Player.UpdatePlayerStatus <| PlayerData.jsToElmPlayerStatus jsPlayerStatus)
                                playerModel
                    in
                    { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

                Ports.JSPlayerCurrTime currTime ->
                    let
                        ( subModel, subCmd ) =
                            Player.update
                                (Player.UpdatePlayerTime currTime)
                                playerModel
                    in
                    { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

        ( PlayerMsg playerMsg, Loaded (Player playerModel) ) ->
            let
                ( subModel, subCmd ) =
                    Player.update playerMsg playerModel
            in
            { model | pageState = Loaded (Player subModel) } ! [ Cmd.map PlayerMsg subCmd ]

        ( PageLoadingMsg loadingMsg, Loading loadingModel ) ->
            let
                subModel =
                    PageLoading.update loadingMsg loadingModel
            in
            { model | pageState = Loading subModel } ! []

        ( _, Loaded NotFound ) ->
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
        [ pageSubscriptions <| model.pageState
        , Ports.pullJSDataToElm PortMsg PortErr
        , Window.resizes WindowResize
        ]


pageSubscriptions : PageState -> Sub Msg
pageSubscriptions pageState =
    case pageState of
        Loading _ ->
            Sub.map PageLoadingMsg <| Time.every (Time.second * 0.5) PageLoading.Tick

        Loaded (Player playerModel) ->
            if playerModel.playerStatus == PlayerData.Playing then
                Sub.map PlayerMsg <| Time.every (Time.second * 0.1) Player.Tick
            else
                Sub.none

        _ ->
            Sub.none


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        Loading _ ->
            Blank



--- VIEW ---


view : Model -> Html Msg
view model =
    let
        masterFrame isLoading =
            Master.frame
                { navOpen = model.navOpen, isLoading = isLoading, menuMsg = ToggleMenu }
    in
    case model.pageState of
        Loading loadingModel ->
            masterFrame True <| PageLoading.loading loadingModel

        Loaded NotFound ->
            masterFrame False <| E.el S.None [] (E.text "*** NotFound ***")

        Loaded Blank ->
            masterFrame False <| E.el S.None [] (E.text "*** Blank ***")

        Loaded (Errored err) ->
            masterFrame False <| Errored.view err

        Loaded (Home homeModel) ->
            masterFrame False <| Home.view homeModel

        Loaded (Player playerModel) ->
            masterFrame False <| E.map PlayerMsg (Player.view playerModel)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Root ->
            model ! [ Route.modifyUrl Route.Home ]

        Just Route.Home ->
            { model | navOpen = False, pageState = Loading PageLoading.getInitModel }
                ! [ Task.attempt HomeLoaded Home.load
                  ]

        Just (Route.Player youTubeID) ->
            { model | navOpen = False, pageState = Loading PageLoading.getInitModel }
                ! [ Task.attempt PlayerLoaded <| Player.load youTubeID
                  ]



--- MAIN ---


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
