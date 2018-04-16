module View.Master exposing (frame)

-- import Page.Master as Master

import Element as E
import Element.Attributes as A
import Html exposing (Html)
import Icons
import Route
import Styles as S


type alias FrameConfig msg =
    { navOpen : Bool
    , isLoading : Bool
    , menuMsg : msg
    }


frame : FrameConfig msg -> E.Element S.Styles variation msg -> Html msg
frame frameConfig pageContent =
    E.viewport S.stylesheet <|
        E.column S.App
            []
            [ E.row S.None
                []
                [ appNav frameConfig.navOpen
                , E.column S.None
                    [ A.width A.fill ]
                    [ topBar frameConfig "Home Page"
                    , E.mainContent S.None [ A.verticalCenter, A.padding 50 ] <| pageContent
                    ]
                ]
            ]


menuToggleAttr : Bool -> List (E.Attribute variation msg)
menuToggleAttr navOpen =
    if navOpen then
        [ A.width <| A.fill ]
    else
        [ A.width <| A.px 0, A.hidden ]


appNav : Bool -> E.Element S.Styles variation msg
appNav navOpen =
    E.navigationColumn S.AppNav
        (menuToggleAttr navOpen)
        { options =
            [ E.el S.Logo [ A.padding 50 ] <| E.text "fivefrets"
            , E.el S.Version [ A.padding 5 ] <| E.el S.None [ A.center ] <| E.text "v 0.0"
            , E.column S.None
                []
                [ E.link (Route.href Route.Home) <| E.el S.NavItem [ A.padding 10 ] <| E.text "Home"

                -- , E.link "/profile" <| E.el S.NavItem [ A.padding 10 ] <| E.text "Profile"
                -- , E.link "/logout" <| E.el S.NavItem [ A.padding 10 ] <| E.text "Logout"
                ]
            ]
        , name = "fivefrets"
        }


topBar : FrameConfig msg -> String -> E.Element S.Styles variation msg
topBar frameConfig title =
    let
        topContent =
            if frameConfig.isLoading then
                "Loading  . . ."
            else
                title
    in
    E.row S.TopBar
        [ A.padding 10, A.spacing 10, A.verticalCenter ]
        [ E.el S.None [ A.alignLeft ] <| menuIcon frameConfig --E.text "X"
        , E.el S.None [ A.spread ] <| E.text (Debug.log "topContent" topContent)
        ]


menuIcon : FrameConfig msg -> E.Element S.Styles variation msg
menuIcon frameConfig =
    if frameConfig.navOpen then
        Icons.closeIconButton frameConfig.menuMsg
    else
        Icons.menuIconButton frameConfig.menuMsg
