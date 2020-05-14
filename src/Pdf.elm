module Pdf exposing (Page, Pdf, addPage, addPages, init, page)

import Length exposing (Length)


type Pdf
    = Pdf ( Page, List Page )


type Page
    = Page { width : Length, height : Length, text : List String }


page : { width : Length, height : Length } -> Page
page { width, height } =
    Page { width = width, height = height, text = [] }


init : Page -> Pdf
init firstPage =
    Pdf ( firstPage, [] )


addPage : Page -> Pdf -> Pdf
addPage page_ (Pdf ( firstPage, rest )) =
    Pdf ( firstPage, rest ++ [ page_ ] )


addPages : List Page -> Pdf -> Pdf
addPages pages (Pdf ( firstPage, rest )) =
    Pdf ( firstPage, rest ++ pages )
