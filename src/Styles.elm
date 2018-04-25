module Styles exposing (ActiveInactive(..), Styles(..), stylesheet)

import ColorPalette exposing (appColors)
import Style as S
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Style.Transition as Transition


type Styles
    = None
    | App
    | Logo
    | Version
    | AppNav
    | NavItem
    | Error
    | Button ActiveInactive
    | IconButton
    | TopBar
    | Title
    | SongItem
    | ChordGridContainer
    | ChordItem ActiveInactive
    | YouTubeSpace
    | LoadingBox


type ActiveInactive
    = Active
    | Inactive


type Disabled
    = Disabled


stylesheet : S.StyleSheet Styles variation
stylesheet =
    S.styleSheet
        [ S.style None []
        , S.style App
            [ Font.typeface <| Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto:300,400", name = "Roboto" } :: []
            , Color.background appColors.background
            , Color.text appColors.primary
            , Font.size 16
            ]
        , S.style AppNav
            []
        , S.style Logo
            [ Color.background appColors.primary
            , Color.text appColors.textOnPrimary
            , Font.letterSpacing 5
            , S.prop "font-variant-ligatures" "none"
            , Font.size 32
            , Font.weight 300
            ]
        , S.style Version
            [ Color.background appColors.primary
            , Color.text appColors.textOnPrimary
            , Font.size 10
            ]
        , S.style NavItem
            [ Border.bottom 3
            , Border.solid
            , Color.border appColors.primary
            , S.hover [ Color.background appColors.secondary ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "background" ]
                  }
                ]
            ]
        , S.style Error
            [ Color.text appColors.error
            , Font.size 48
            , Font.weight 300
            ]
        , S.style (Button Active)
            [ Color.border appColors.primary
            , Color.background appColors.lightPrimary
            , Color.text appColors.primary
            , Border.all 2
            , Shadow.simple
            , S.hover [ Color.background appColors.secondary ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "background" ]
                  }
                ]
            ]
        , S.style (Button Inactive)
            [ Color.border appColors.lightPrimary
            , Color.text appColors.lightPrimary
            , Border.all 2
            , Shadow.simple
            , S.cursor "not-allowed"
            ]
        , S.style IconButton
            [ Color.border appColors.primary
            , Color.background appColors.lightPrimary
            , Color.text appColors.primary

            -- , S.hover [ S.rotate pi ]
            -- , Transition.transitions
            --     [ { delay = 0
            --       , duration = 500
            --       , easing = "ease-in-out"
            --       , props = [ "transform" ]
            --       }
            --     ]
            ]
        , S.style TopBar
            [ Color.background appColors.primary
            , Color.text appColors.textOnPrimary
            , Font.lineHeight 2
            ]
        , S.style Title
            [ Font.size 32
            , Color.text appColors.primary
            ]
        , S.style SongItem
            [ Color.background appColors.lightPrimary
            , Shadow.deep
            , S.cursor "pointer"
            , S.hover [ Shadow.simple ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "box-shadow" ]
                  }
                ]
            ]
        , S.style ChordGridContainer
            []
        , S.style (ChordItem Inactive)
            [ Color.background appColors.lightPrimary
            , Shadow.deep
            , S.cursor "pointer"
            ]
        , S.style (ChordItem Active)
            [ Color.background appColors.secondary
            , Shadow.simple
            ]
        , S.style YouTubeSpace []
        , S.style LoadingBox
            [ Color.background appColors.primary
            , Transition.transitions
                [ { delay = 0
                  , duration = 1000
                  , easing = "ease-in-out"
                  , props = [ "height" ]
                  }
                ]
            ]
        ]



--- HELPER FN ---


baseButtonStyles : List (S.Property class variation)
baseButtonStyles =
    [ Color.border appColors.primary
    , Color.background appColors.lightPrimary
    , Color.text appColors.primary
    , Border.all 2
    , Shadow.simple
    , S.hover [ Color.background appColors.secondary ]
    , Transition.transitions
        [ { delay = 0
          , duration = 500
          , easing = "ease-in-out"
          , props = [ "background" ]
          }
        ]
    ]
