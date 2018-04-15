module View.Master exposing (frame)

import Element as E
import Element.Attributes as A
import Html exposing (Html)
import Route
import Styles as S


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
