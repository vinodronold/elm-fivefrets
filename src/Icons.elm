module Icons exposing (closeIconButton, menuIconButton)

import ColorPalette exposing (appColors)
import Element as E exposing (Element)
import Element.Attributes as A
import Element.Events as Events
import Material.Icons.Navigation exposing (close, menu)
import Styles as S


closeIconButton : msg -> Element S.Styles variation msg
closeIconButton msg =
    iconButton closeIcon msg


menuIconButton : msg -> Element S.Styles variation msg
menuIconButton msg =
    iconButton meunIcon msg


iconButton : Icon variation msg -> msg -> Element S.Styles variation msg
iconButton icon msg =
    E.button S.IconButton [ A.height <| A.px 32, Events.onClick msg ] <| icon 32


type alias Icon variation msg =
    Int -> Element S.Styles variation msg


closeIcon : Icon variation msg
closeIcon size =
    close appColors.textOnPrimary size
        |> E.html


meunIcon : Icon variation msg
meunIcon size =
    menu appColors.textOnPrimary size
        |> E.html
