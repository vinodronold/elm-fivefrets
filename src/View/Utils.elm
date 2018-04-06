module View.Utils exposing (..)

import Element as E
import Element.Attributes as A
import Element.Events as Event
import Styles as S


disabledButton : String -> E.Element S.Styles variation msg
disabledButton label =
    E.el (S.Button S.Inactive) [ A.paddingXY 10 5 ] <| E.text label


button : String -> msg -> E.Element S.Styles variation msg
button label msg =
    E.button (S.Button S.Active) [ A.paddingXY 10 5, Event.onClick msg ] <| E.text label
