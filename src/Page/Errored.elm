module Page.Errored exposing (PageLoadError, pageLoadError, pageLoadErrorToString, view)

import Element as E
import Element.Attributes as A
import Styles as S


type PageLoadError
    = PageLoadError String


pageLoadError : String -> PageLoadError
pageLoadError =
    PageLoadError


pageLoadErrorToString : PageLoadError -> String
pageLoadErrorToString (PageLoadError str) =
    str


view : PageLoadError -> E.Element S.Styles variation msg
view (PageLoadError errMessage) =
    E.el S.Error [ A.verticalCenter, A.center ] (E.text errMessage)
