module App.Utils exposing (button)

import App.Model as M
import App.Styles as S
import Element as E
import Element.Attributes as A
import Element.Events as E


--- VIEW UTLIS FN ---


button : String -> M.Msg -> E.Element S.Styles variation M.Msg
button label msg =
    E.button S.Button [ A.paddingXY 10 5, E.onClick msg ] <| E.text label
