module ColorPalette exposing (appColors)

import Color exposing (rgba)


type alias AppColorPalette =
    { primary : Color.Color
    , lightPrimary : Color.Color
    , textOnPrimary : Color.Color
    , secondary : Color.Color
    , background : Color.Color
    , error : Color.Color
    }


primaryRgb : Float -> Color.Color
primaryRgb =
    rgba 38 50 56


appColors : AppColorPalette
appColors =
    { primary = primaryRgb 1
    , lightPrimary = primaryRgb 0.3
    , textOnPrimary = rgba 255 255 255 0.87
    , secondary = rgba 255 212 84 1
    , background = rgba 255 255 255 1
    , error = rgba 213 0 0 0.87
    }
