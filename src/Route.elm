module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.Song exposing (SongID, songIDParser, songIDtoString)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


type Route
    = Home
    | Root
    | Player SongID


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Player (s "player" </> songIDParser)
        ]



-- INTERNAL --
-- routeToString : Route -> String
-- routeToString page =
--     let
--         pieces =
--             case page of
--                 Home ->
--                     []
--                 Root ->
--                     []
--                 Player songID ->
--                     [ "player", songIDtoString songID ]
--     in
--     "#/" ++ String.join "/" pieces
-- PUBLIC HELPERS --


href : Route -> String
href page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Player songID ->
                    [ "player", songIDtoString songID ]
    in
    "#/" ++ String.join "/" pieces


modifyUrl : Route -> Cmd msg
modifyUrl =
    href >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        parseHash route location
