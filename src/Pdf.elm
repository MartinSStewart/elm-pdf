module Pdf exposing
    ( pdf, page, encoder, Pdf, Page
    , text, image, Item, PageCoordinates
    , helvetica, timesRoman, courier, symbol, zapfDingbats, Font
    )

{-| In order to use this package you'll need to install
[`ianmackenzie/elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/)
, [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)
, and [`elm/bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/).


# PDF creation

@docs pdf, page, textBox, encoder, Pdf, Page


# Page content

@docs text, image, Item, PageCoordinates


# Built-in fonts

@docs helvetica, timesRoman, courier, symbol, zapfDingbats, Font

-}

import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Dict exposing (Dict)
import Flate
import Length exposing (Length, Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Round
import Set exposing (Set)
import Vector2d exposing (Vector2d)


{-| The coordinate system used when placing things on a page.
-}
type PageCoordinates
    = PageCoordinates Never


type Pdf
    = Pdf { title : String, pages : List Page }


type Page
    = Page (Vector2d Meters PageCoordinates) (List Item)


type Item
    = TextItem
        { position : Point2d Meters PageCoordinates
        , font : Font
        , text : String
        , fontSize : Length
        }
    | RasterImage
        { boundingBox : BoundingBox2d Meters PageCoordinates
        , size : ( Quantity Int Pixels, Quantity Int Pixels )
        , image : Bytes
        }


text : Length -> Font -> Point2d Meters PageCoordinates -> String -> Item
text fontSize font position text_ =
    TextItem
        { position = position
        , font = font
        , text = text_
        , fontSize = fontSize
        }


image : BoundingBox2d Meters PageCoordinates -> ( Quantity Int Pixels, Quantity Int Pixels ) -> Bytes -> Item
image boundingBox size image_ =
    RasterImage
        { boundingBox = boundingBox
        , size = size
        , image = image_
        }


page : { size : Vector2d Meters PageCoordinates, contents : List Item } -> Page
page { size, contents } =
    Page size contents


pdf : String -> List Page -> Pdf
pdf title_ pages_ =
    Pdf
        { title = title_
        , pages = pages_
        }


title : Pdf -> String
title (Pdf pdf_) =
    pdf_.title


pages : Pdf -> List Page
pages (Pdf pdf_) =
    pdf_.pages



--- ENCODE ---


encoder : Pdf -> BE.Encoder
encoder pdf_ =
    let
        infoIndirectReference =
            { index = 1, revision = 0 }

        catalogIndirectReference =
            { index = 2, revision = 0 }

        pageRootIndirectReference =
            { index = 3, revision = 0 }

        info : IndirectObject
        info =
            indirectObject
                infoIndirectReference
                (PdfDict [ ( "Title", Text (title pdf_) ) ])

        catalog : IndirectObject
        catalog =
            entryPoint
                catalogIndirectReference
                (PdfDict [ ( "Type", Name "Catalog" ), ( "Pages", IndirectReference pageRootIndirectReference ) ])

        pageRoot : List Font -> Int -> IndirectObject
        pageRoot fonts fontIndexOffset =
            indirectObject
                pageRootIndirectReference
                ([ ( "Kids"
                   , allPages_
                        |> List.map (.page >> indirectObjectToIndirectReference >> IndirectReference)
                        |> PdfArray
                   )
                 , ( "Count", PdfInt (List.length (pages pdf_)) )
                 , ( "Type", Name "Pages" )
                 , ( "Resources"
                   , PdfDict
                        [ ( "Font"
                          , List.indexedMap
                                (\index font ->
                                    ( "F" ++ String.fromInt (index + 1)
                                    , IndirectReference { index = index + fontIndexOffset, revision = 0 }
                                    )
                                )
                                fonts
                                |> PdfDict
                          )
                        , ( "PRocSet", PdfArray [ Name "PDF", Name "Text" ] )
                        ]
                   )
                 ]
                    |> PdfDict
                )

        allPages_ : List { page : IndirectObject, content : IndirectObject }
        allPages_ =
            pageObjects usedFonts pageRootIndirectReference (List.length usedFonts + 4) (pages pdf_)

        usedFonts : List Font
        usedFonts =
            pages pdf_
                |> List.concatMap
                    (\(Page _ items) ->
                        List.concatMap
                            (\item ->
                                case item of
                                    TextItem { font } ->
                                        [ font ]

                                    RasterImage _ ->
                                        []
                            )
                            items
                    )
                |> uniqueBy fontName

        ( content, xRef ) =
            info
                :: catalog
                :: pageRoot usedFonts 4
                :: List.indexedMap (\index font -> fontObject { index = index + 4, revision = 0 } font) usedFonts
                ++ List.concatMap (\a -> [ a.page, a.content ]) allPages_
                |> contentToBytes

        xRefToString : XRef -> BE.Encoder
        xRefToString (XRef xRefs) =
            let
                xRefLine { offset } =
                    String.padLeft 10 '0' (String.fromInt offset)
                        ++ " "
                        ++ "00000"
                        ++ " n\n"

                xRefCount =
                    List.length xRefs + 1
            in
            BE.sequence
                [ "xref\n"
                    ++ ("0 " ++ String.fromInt xRefCount ++ "\n")
                    ++ "0000000000 65535 f\n"
                    ++ (List.map xRefLine xRefs |> String.concat)
                    ++ "trailer\n"
                    |> BE.string
                , pdfDictToString
                    [ ( "Size", PdfInt xRefCount )
                    , ( "Info", IndirectReference infoIndirectReference )
                    , ( "Root", IndirectReference catalogIndirectReference )
                    ]
                , BE.string "\nstartxref\n"
                ]
    in
    BE.sequence
        [ BE.bytes content
        , xRefToString (XRef xRef)
        , String.fromInt (Bytes.width content) |> BE.string
        , BE.string "\n%%EOF"
        ]


{-| Original code from elm-community/list-extra.
Copied here so we don't need an entire dependency for a small portion of the API.
-}
uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelp f Set.empty list []


uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if Set.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)


fontObject : IndirectReference_ -> Font -> IndirectObject
fontObject indirectReference font_ =
    indirectObject
        indirectReference
        (PdfDict
            [ ( "Type", Name "Font" )
            , ( "Subtype", Name "Type1" )
            , ( "BaseFont", Name (fontName font_) )
            , ( "Encoding", Name "WinAnsiEncoding" )
            ]
        )


pageObjects : List Font -> IndirectReference_ -> Int -> List Page -> List { page : IndirectObject, content : IndirectObject }
pageObjects fonts pageRootIndirectReference indexStart pages_ =
    let
        fontLookup =
            List.indexedMap (\index font -> ( fontName font, index + 1 )) fonts
                |> Dict.fromList
    in
    pages_
        |> List.indexedMap
            (\index (Page pageSize pageText) ->
                let
                    contentIndirectReference =
                        { index = index * 2 + indexStart + 1, revision = 0 }

                    streamContent : StreamContent
                    streamContent =
                        List.foldl
                            (\item previous ->
                                case item of
                                    TextItem text_ ->
                                        let
                                            ( x, y ) =
                                                Point2d.toTuple Length.inPoints text_.position

                                            writeLine : String -> String
                                            writeLine line =
                                                " " ++ textToString line ++ " Tj"

                                            lineSpacing : String
                                            lineSpacing =
                                                lengthToString (Quantity.negate text_.fontSize)
                                        in
                                        case String.lines text_.text of
                                            head :: rest ->
                                                let
                                                    restOfLines : String
                                                    restOfLines =
                                                        rest
                                                            |> List.map
                                                                (\line ->
                                                                    (" 0 " ++ lineSpacing ++ " Td") ++ writeLine line
                                                                )
                                                            |> String.concat

                                                    fontIndex =
                                                        Dict.get (fontName text_.font) fontLookup |> Maybe.withDefault 1

                                                    setFont =
                                                        " /F"
                                                            ++ String.fromInt fontIndex
                                                            ++ " "
                                                            ++ lengthToString text_.fontSize
                                                            ++ " Tf "
                                                in
                                                previous
                                                    ++ setFont
                                                    ++ (floatToString x ++ " " ++ floatToString y ++ " Td")
                                                    ++ writeLine head
                                                    ++ restOfLines

                                            [] ->
                                                ""

                                    RasterImage _ ->
                                        Debug.todo ""
                            )
                            ""
                            pageText
                            |> (\a -> "BT" ++ a ++ " ET")
                            |> DrawingInstructions
                in
                { page =
                    indirectObject
                        { index = index * 2 + indexStart, revision = 0 }
                        (PdfDict
                            [ ( "Type", Name "Page" )
                            , mediaBox pageSize
                            , ( "Parent", IndirectReference pageRootIndirectReference )
                            , ( "Contents", IndirectReference contentIndirectReference )
                            ]
                        )
                , content =
                    indirectObject
                        contentIndirectReference
                        (Stream [] streamContent)
                }
            )


contentToBytes : List IndirectObject -> ( Bytes, List { offset : Int } )
contentToBytes =
    List.sortBy indirectObjectIndex
        >> List.foldl
            (\indirectObject_ ( content_, xRef_, index ) ->
                ( BE.sequence [ BE.bytes content_, indirectObjectToString indirectObject_, BE.string "\n" ] |> BE.encode
                , { offset = Bytes.width content_ } :: xRef_
                , index + 1
                )
            )
            ( header
            , []
            , 1
            )
        >> (\( content, xRef, _ ) -> ( content, List.reverse xRef ))


header =
    BE.sequence
        [ "%PDF-" ++ pdfVersion ++ "\n%" |> BE.string

        -- Comment containing 4 ascii encoded é's to indicate that this pdf file contains binary data
        , BE.unsignedInt8 233
        , BE.unsignedInt8 233
        , BE.unsignedInt8 233
        , BE.unsignedInt8 233
        , BE.string "\n"
        ]
        |> BE.encode


pdfVersion =
    "1.7"


mediaBox : Vector2d Meters PageCoordinates -> ( String, Object )
mediaBox size =
    let
        ( w, h ) =
            Vector2d.toTuple Length.inPoints size
    in
    ( "MediaBox", PdfArray [ PdfInt 0, PdfInt 0, PdfFloat w, PdfFloat h ] )


type XRef
    = XRef (List { offset : Int })


type Object
    = Name String
    | PdfFloat Float
    | PdfInt Int
    | PdfDict (List ( String, Object ))
    | PdfArray (List Object)
    | Text String
    | Stream (List ( String, Object )) StreamContent
    | IndirectReference IndirectReference_


objectToString : Object -> BE.Encoder
objectToString object =
    case object of
        Name name ->
            nameToString name |> BE.string

        PdfFloat float ->
            floatToString float |> BE.string

        PdfInt int ->
            String.fromInt int |> BE.string

        PdfDict pdfDict ->
            pdfDictToString pdfDict

        PdfArray pdfArray ->
            let
                contentText =
                    List.map objectToString pdfArray |> List.intersperse (BE.string " ")
            in
            BE.string "[ " :: contentText ++ [ BE.string " ]" ] |> BE.sequence

        Text text_ ->
            textToString text_ |> BE.string

        Stream dict streamContent ->
            let
                ( streamContent_, dict2 ) =
                    case streamContent of
                        ResourceData data ->
                            ( data, ( "Length", PdfInt (Bytes.width data) ) :: dict )

                        DrawingInstructions text_ ->
                            let
                                textBytes =
                                    text_ |> BE.string |> BE.encode |> Flate.deflateZlib
                            in
                            ( textBytes
                            , ( "Length", PdfInt (Bytes.width textBytes) )
                                :: ( "Filter", Name "FlateDecode" )
                                :: dict
                            )
            in
            BE.sequence
                [ pdfDictToString dict2
                , BE.string "\nstream\n"
                , BE.bytes streamContent_
                , BE.string "\nendstream"
                ]

        IndirectReference { index, revision } ->
            String.fromInt index ++ " " ++ String.fromInt revision ++ " R" |> BE.string


textToString : String -> String
textToString text_ =
    text_
        -- Convert windows line endings to unix line endings
        |> String.replace "\u{000D}\n" "\n"
        -- Escape backslashes
        |> String.replace "\\" "\\\\"
        -- Escape parenthesis
        |> String.replace ")" "\\)"
        |> String.replace "(" "\\("
        |> (\a -> "(" ++ a ++ ")")


nameToString : String -> String
nameToString name =
    "/" ++ name


pdfDictToString : List ( String, Object ) -> BE.Encoder
pdfDictToString =
    List.map (\( key, value ) -> BE.sequence [ BE.string (nameToString key ++ " "), objectToString value ])
        >> List.intersperse (BE.string " ")
        >> (\a -> BE.string "<< " :: a ++ [ BE.string " >>" ])
        >> BE.sequence


type alias IndirectReference_ =
    { index : Int, revision : Int }


type IndirectObject
    = IndirectObject { index : Int, revision : Int, isEntryPoint : Bool, object : Object }


indirectObject : IndirectReference_ -> Object -> IndirectObject
indirectObject { index, revision } object =
    IndirectObject { index = index, revision = revision, isEntryPoint = False, object = object }


entryPoint : IndirectReference_ -> Object -> IndirectObject
entryPoint { index, revision } object =
    IndirectObject { index = index, revision = revision, isEntryPoint = True, object = object }


indirectObjectToIndirectReference : IndirectObject -> IndirectReference_
indirectObjectToIndirectReference (IndirectObject { index, revision }) =
    { index = index, revision = revision }


indirectObjectToString : IndirectObject -> BE.Encoder
indirectObjectToString (IndirectObject { index, revision, isEntryPoint, object }) =
    BE.sequence
        [ String.fromInt index
            ++ " "
            ++ String.fromInt revision
            ++ " obj"
            ++ (if isEntryPoint then
                    "  % entry point\n"

                else
                    "\n"
               )
            |> BE.string
        , objectToString object
        , BE.string "\nendobj"
        ]


indirectObjectIndex : IndirectObject -> Int
indirectObjectIndex (IndirectObject { index }) =
    index


type StreamContent
    = ResourceData Bytes
    | DrawingInstructions String


floatToString : Float -> String
floatToString =
    Round.round 5


lengthToString : Length -> String
lengthToString =
    Length.inPoints >> floatToString


type Font
    = Courier { bold : Bool, oblique : Bool }
    | Helvetica { bold : Bool, oblique : Bool }
    | TimesRoman { bold : Bool, italic : Bool }
    | Symbol
    | ZapfDingbats


courier : { bold : Bool, oblique : Bool } -> Font
courier { bold, oblique } =
    Courier { bold = bold, oblique = oblique }


helvetica : { bold : Bool, oblique : Bool } -> Font
helvetica { bold, oblique } =
    Helvetica { bold = bold, oblique = oblique }


timesRoman : { bold : Bool, italic : Bool } -> Font
timesRoman { bold, italic } =
    TimesRoman { bold = bold, italic = italic }


symbol : Font
symbol =
    Symbol


zapfDingbats : Font
zapfDingbats =
    ZapfDingbats


fontName : Font -> String
fontName font =
    case font of
        Courier { bold, oblique } ->
            case ( bold, oblique ) of
                ( False, False ) ->
                    "Courier"

                ( True, False ) ->
                    "Courier-Bold"

                ( False, True ) ->
                    "Courier-Oblique"

                ( True, True ) ->
                    "Courier-BoldOblique"

        Helvetica { bold, oblique } ->
            case ( bold, oblique ) of
                ( False, False ) ->
                    "Helvetica"

                ( True, False ) ->
                    "Helvetica-Bold"

                ( False, True ) ->
                    "Helvetica-Oblique"

                ( True, True ) ->
                    "Helvetica-BoldOblique"

        TimesRoman { bold, italic } ->
            case ( bold, italic ) of
                ( False, False ) ->
                    "Times-Roman"

                ( True, False ) ->
                    "Times-Bold"

                ( False, True ) ->
                    "Times-Italic"

                ( True, True ) ->
                    "Times-BoldItalic"

        Symbol ->
            "Symbol"

        ZapfDingbats ->
            "ZapfDingbats"
