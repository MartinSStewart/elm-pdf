module Pdf exposing (Page, Pdf, TextBox, addPage, addPages, encode, init, page, pages, title)

import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Round
import Vector2d exposing (Vector2d)


type PageCoordinates
    = PageCoordinates Never


type Pdf
    = Pdf { title : String, firstPage : Page, restOfPages : List Page }


type Page
    = Page (Vector2d Meters PageCoordinates) (List TextBox)


type TextBox
    = TextBox
        { position : Point2d Meters PageCoordinates
        , maxWidth : Maybe Length
        , text : String
        }


page : Vector2d Meters PageCoordinates -> List TextBox -> Page
page =
    Page


init : { title : String, firstPage : Page } -> Pdf
init record =
    Pdf
        { title = record.title
        , firstPage = record.firstPage
        , restOfPages = []
        }


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



--- ENCODE ---


encode : Pdf -> String
encode pdf =
    let
        info =
            PdfDict [ ( Name "Title", Text_ (Text (title pdf)) ) ] |> PdfDict_

        catalog =
            PdfDict
                [ ( Name "Type", Name_ (Name "Catalog") )
                , ( Name "Pages", IndirectReference_ (IndirectReference { index = 3, revision = 0 }) )
                ]
                |> PdfDict_

        allPages =
            pages pdf
                |> List.map
                    (\(Page pageSize pageText) ->
                        PdfDict [ ( Name "Type", Name_ (Name "Page") ), mediaBox pageSize ]
                            |> PdfDict_
                    )

        pages_ =
            PdfDict
                [ ( Name "Kids", PdfArray_ (PdfArray [ IndirectReference_ (IndirectReference { index = 0, revision = 0 }) ]) )
                , ( Name "Count", Int_ (List.length allPages) )
                , ( Name "Pages", IndirectReference_ (IndirectReference { index = 3, revision = 0 }) )
                ]
                |> PdfDict_

        ( content, xRef, _ ) =
            [ info, catalog, pages_ ]
                |> List.foldl
                    (\object ( content_, xRef_, index ) ->
                        let
                            text_ =
                                indirectObjectToString index 0 object
                        in
                        ( content_ ++ text_ ++ "\n"
                        , { offset = String.length content_, size = String.length text_ } :: xRef_
                        , index + 1
                        )
                    )
                    ( "%PDF-1.7\n", [], 1 )
    in
    content
        ++ xRefToString 1 2 (XRef (List.reverse xRef))
        ++ String.fromInt (String.length content)
        ++ "\n%%EOF"


mediaBox : Vector2d units coordinates -> ( Name, Object )
mediaBox size =
    let
        ( w, h ) =
            Vector2d.toTuple Length.inPoints size
    in
    ( Name "MediaBox", PdfArray_ (PdfArray [ Int_ 0, Int_ 0, Float_ w, Float_ h ]) )


type XRef
    = XRef (List { offset : Int, size : Int })


xRefToString : Int -> Int -> XRef -> String
xRefToString infoIndex rootIndex (XRef xRefs) =
    let
        xRefLine { offset, size } =
            String.padLeft 10 '0' (String.fromInt offset)
                ++ " "
                ++ String.padLeft 5 '0' (String.fromInt size)
                ++ " n\n"
    in
    "xref\n"
        ++ ("0 " ++ String.fromInt (List.length xRefs) ++ "\n")
        ++ "0000000000 65535 f\n"
        ++ (List.map xRefLine xRefs |> String.concat)
        ++ "trailer\n"
        ++ dictionaryToString
            (PdfDict
                [ ( Name "Size", Int_ (List.length xRefs) )
                , ( Name "Info", IndirectReference_ (IndirectReference { index = infoIndex, revision = 0 }) )
                , ( Name "Root", IndirectReference_ (IndirectReference { index = rootIndex, revision = 0 }) )
                ]
            )
        ++ "\nstartxref\n"


type Name
    = Name String


type Object
    = Name_ Name
    | Float_ Float
    | Int_ Int
    | PdfDict_ PdfDict
    | PdfArray_ PdfArray
    | Text_ Text
    | Stream_ Stream
    | IndirectReference_ IndirectReference


objectToString : Object -> String
objectToString object =
    case object of
        Name_ name ->
            nameToString name

        Float_ float ->
            floatToString float

        Int_ int ->
            String.fromInt int

        PdfDict_ pdfDict ->
            dictionaryToString pdfDict

        PdfArray_ pdfArray ->
            arrayToString pdfArray

        Text_ text_ ->
            textToString text_

        Stream_ stream ->
            streamToString stream

        IndirectReference_ indirectReference ->
            indirectReferenceToString indirectReference


nameToString : Name -> String
nameToString (Name name) =
    "/" ++ name


type PdfDict
    = PdfDict (List ( Name, Object ))


dictionaryToString : PdfDict -> String
dictionaryToString (PdfDict keyValues) =
    let
        contentText =
            keyValues
                |> List.map (\( key, value ) -> nameToString key ++ " " ++ objectToString value)
                |> List.intersperse " "
                |> String.concat
    in
    "<< " ++ contentText ++ " >>"


type Stream
    = Stream PdfDict StreamContent


type IndirectObject
    = IndirectObject


indirectObjectToString : Int -> Int -> Object -> String
indirectObjectToString index revision object =
    String.fromInt index
        ++ " "
        ++ String.fromInt revision
        ++ " obj\n"
        ++ objectToString object
        ++ "\nendobject"


type IndirectReference
    = IndirectReference { index : Int, revision : Int }


indirectReferenceToString : IndirectReference -> String
indirectReferenceToString (IndirectReference { index, revision }) =
    String.fromInt index ++ " " ++ String.fromInt revision ++ " R"


type StreamContent
    = StreamContent String


streamToString : Stream -> String
streamToString (Stream (PdfDict dict) (StreamContent content)) =
    let
        dict2 =
            ( Name "Length", Int_ (String.length content + 2) ) :: dict |> PdfDict
    in
    dictionaryToString dict2 ++ " stream " ++ content ++ " endstream"


type Text
    = Text String


text : String -> Text
text =
    Text


textToString : Text -> String
textToString (Text text_) =
    text_
        |> String.replace "\\" "\\\\"
        |> String.replace ")" "\\)"
        |> String.replace "(" "\\("
        |> (\a -> "(" ++ a ++ ")")


type PdfArray
    = PdfArray (List Object)


arrayToString : PdfArray -> String
arrayToString (PdfArray content) =
    let
        contentText =
            List.map objectToString content |> List.intersperse " " |> String.concat
    in
    "[ " ++ contentText ++ " ]"


floatToString : Float -> String
floatToString =
    Round.round 5
