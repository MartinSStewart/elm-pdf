module Pdf exposing (Page, Pdf, addPage, addPages, init, page, pages, title)

import Length exposing (Length, Meters)
import Point2d exposing (Point2d)


type PageCoordinates
    = PageCoordinates Never


type Pdf
    = Pdf { firstPage : Page, restOfPages : List Page, title : String }


type Page
    = Page { width : Length, height : Length, text : List TextBox }


type TextBox
    = TextBox
        { position : Point2d Meters PageCoordinates
        , maxWidth : Maybe Length
        , text : String
        }


page : { width : Length, height : Length } -> Page
page { width, height } =
    Page { width = width, height = height, text = [] }


init : String -> Page -> Pdf
init title_ firstPage =
    Pdf { title = title_, firstPage = firstPage, restOfPages = [] }


addPage : Page -> Pdf -> Pdf
addPage page_ (Pdf pdf) =
    Pdf { pdf | restOfPages = pdf.restOfPages ++ [ page_ ] }


addPages : List Page -> Pdf -> Pdf
addPages pages_ (Pdf pdf) =
    Pdf { pdf | restOfPages = pdf.restOfPages ++ pages_ }


title : Pdf -> String
title (Pdf pdf) =
    pdf.title


pages : Pdf -> List Page
pages (Pdf pdf) =
    pdf.firstPage :: pdf.restOfPages
