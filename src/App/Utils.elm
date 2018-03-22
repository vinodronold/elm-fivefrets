module App.Utils exposing (button, disabledButton)

import App.Model as M
import App.Styles as S
import Element as E
import Element.Attributes as A
import Element.Events as E


--- VIEW UTLIS FN ---


disabledButton : String -> E.Element S.Styles variation msg
disabledButton label =
    E.el (S.Button S.Inactive) [ A.paddingXY 10 5 ] <| E.text label


button : String -> M.Msg -> E.Element S.Styles variation M.Msg
button label msg =
    E.button (S.Button S.Active) [ A.paddingXY 10 5, E.onClick msg ] <| E.text label
