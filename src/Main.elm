module Main exposing (..)

import App.Model as M
import App.View as V
import Html exposing (Html)


main : Program Never M.Model M.Msg
main =
    Html.program
        { view = V.view
        , init = M.init
        , update = M.update
        , subscriptions = M.subscriptions
        }
