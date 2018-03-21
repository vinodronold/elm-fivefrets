module App.Styles exposing (ActiveInactive(..), Styles(..), stylesheet)

import Color exposing (rgba)
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
    | NavItem
    | Button
    | TopBar
    | SongItem
    | ChordItem ActiveInactive


type ActiveInactive
    = Active
    | Inactive


stylesheet : S.StyleSheet Styles variation
stylesheet =
    S.styleSheet
        [ S.style None []
        , S.style App
            [ Font.typeface <| Font.importUrl { url = "https://fonts.googleapis.com/css?family=Roboto", name = "Roboto" } :: []
            , Color.background appColor.background
            , Color.text appColor.primary
            , Font.size 16
            ]
        , S.style Logo
            [ Color.background appColor.primary
            , Color.text appColor.textOnPrimary
            , Font.letterSpacing 5
            , S.prop "font-variant-ligatures" "none"
            , Font.size 32
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
        , S.style Button
            [ Color.border appColor.primary
            , Color.background appColor.lightPrimary
            , Color.text appColor.primary
            , Border.all 2
            , Shadow.simple
            , S.hover [ Color.background appColor.secondary ]
            , Transition.transitions
                [ { delay = 0
                  , duration = 500
                  , easing = "ease-in-out"
                  , props = [ "background" ]
                  }
                ]
            ]
        , S.style TopBar
            [ Color.background appColor.primary
            , Color.text appColor.textOnPrimary
            , Font.lineHeight 2
            ]
        , S.style SongItem
            [ Color.background appColor.lightPrimary
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
        , S.style (ChordItem Inactive)
            [ Color.background appColor.lightPrimary
            , Shadow.deep
            ]
        , S.style (ChordItem Active)
            [ Color.background appColor.secondary
            , Shadow.simple
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
    , lightPrimary = primaryRgb 0.3
    , textOnPrimary = rgba 255 255 255 0.87
    , secondary = rgba 255 212 84 1
    , background = rgba 255 255 255 1
    }
