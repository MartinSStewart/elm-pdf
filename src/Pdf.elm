module Pdf exposing
    ( pdf, page, paperSize, encoder, Pdf, Page
    , text, jpgImage, Item, PageCoordinates
    , helvetica, timesRoman, courier, symbol, zapfDingbats, Font
    , ASizes(..), Orientation(..)
    )

{-| In order to use this package you'll need to install
[`ianmackenzie/elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/)
, [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)
, and [`elm/bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/).


# PDF creation

@docs pdf, page, paperSize, encoder, Pdf, Page


# Page content

The content to show on a page.
Currently only text can be shown and a lot of features are missing such as line automatic line breaks and unicode support.

@docs text, jpgImage, Item, PageCoordinates


# Built-in fonts

There are a few fonts that PDF supports by default.
Custom fonts have to be embedded in the file in order to be used and this package doesn't support that yet.

@docs helvetica, timesRoman, courier, symbol, zapfDingbats, Font

-}

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
The top left corner is the origin point.
Larger x values moves you to the right and larger y values move you down the page.
-}
type PageCoordinates
    = PageCoordinates Never


{-| -}
type Pdf
    = Pdf { title : String, images : Dict ImageId JpgImage_, pages : List Page }


{-| -}
type Page
    = Page (Vector2d Meters PageCoordinates) (List Item)


type Item
    = TextItem
        { position : Point2d Meters PageCoordinates
        , font : Font
        , text : String
        , fontSize : Length
        }
    | JpgImage { boundingBox : BoundingBox2d Meters PageCoordinates, imageId : ImageId }


type alias JpgImage_ =
    { size : ( Quantity Int Pixels, Quantity Int Pixels )
    , jpgData : Bytes
    }


{-| Text displayed on a page.
-}
text : Length -> Font -> Point2d Meters PageCoordinates -> String -> Item
text fontSize font position text_ =
    TextItem
        { position = position
        , font = font
        , text = text_
        , fontSize = fontSize
        }


type alias ImageId =
    String


jpgImage : BoundingBox2d Meters PageCoordinates -> ImageId -> Item
jpgImage bounds imageId =
    JpgImage
        { boundingBox = bounds
        , imageId = imageId
        }


{-| A page in our PDF document.
-}
page : { size : Vector2d Meters PageCoordinates, contents : List Item } -> Page
page { size, contents } =
    let
        { x, y } =
            Vector2d.unwrap size
    in
    Page (Vector2d.unsafe { x = max 0 x, y = max 0 y }) contents


{-| [Standard sizes for paper](https://en.wikipedia.org/wiki/ISO_216).
Smaller numbers mean larger sizes.
If you printed something on a typical home printer then it was probably on A4 sized paper.
-}
type ASizes
    = A0
    | A1
    | A2
    | A3
    | A4
    | A5
    | A6
    | A7
    | A8
    | A9
    | A10


{-| -}
type Orientation
    = Landscape
    | Portrait


{-| Typical sizes for paper in physical units.

    paperSize A4 Portrait -- Vector2d.millimeters 210 297

    paperSize A4 Landscape -- Vector2d.millimeters 297 210

    paperSize A0 Landscape -- Vector2d.millimeters 1189 841

-}
paperSize : Orientation -> ASizes -> Vector2d Meters PageCoordinates
paperSize orientation size =
    let
        v =
            case size of
                A0 ->
                    Vector2d.millimeters 841 1189

                A1 ->
                    Vector2d.millimeters 594 841

                A2 ->
                    Vector2d.millimeters 420 594

                A3 ->
                    Vector2d.millimeters 297 420

                A4 ->
                    Vector2d.millimeters 210 297

                A5 ->
                    Vector2d.millimeters 148 210

                A6 ->
                    Vector2d.millimeters 105 148

                A7 ->
                    Vector2d.millimeters 74 105

                A8 ->
                    Vector2d.millimeters 52 74

                A9 ->
                    Vector2d.millimeters 37 52

                A10 ->
                    Vector2d.millimeters 26 37
    in
    case orientation of
        Landscape ->
            Vector2d.unwrap v |> (\{ x, y } -> Vector2d.unsafe { x = y, y = x })

        Portrait ->
            v


{-| Create a PDF.

    import Pdf

    Pdf.pdf "My PDF" [ Pdf.page ]

-}
pdf : String -> Dict String JpgImage_ -> List Page -> Pdf
pdf title_ images_ pages_ =
    Pdf
        { title = title_
        , images = images_
        , pages = pages_
        }


title : Pdf -> String
title (Pdf pdf_) =
    pdf_.title


pages : Pdf -> List Page
pages (Pdf pdf_) =
    pdf_.pages


images : Pdf -> Dict ImageId JpgImage_
images (Pdf pdf_) =
    pdf_.images



--- ENCODE ---


{-| An encoder for converting the PDF to binary data.

    import Bytes exposing (Bytes)
    import Bytes.Encode
    import Pdf

    output : Bytes
    output =
        Pdf.encoder myPdf |> Bytes.Encode.encode

-}
encoder : Pdf -> BE.Encoder
encoder pdf_ =
    let
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

        fontOffset : Int
        fontOffset =
            4

        allPages_ : List { page : IndirectObject, content : IndirectObject }
        allPages_ =
            pageObjects
                usedFonts
                (images pdf_)
                (Dict.size (images pdf_) + List.length usedFonts + fontOffset)
                (pages pdf_)

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

                                    JpgImage _ ->
                                        []
                            )
                            items
                    )
                |> uniqueBy fontName

        ( content, xRef ) =
            info
                :: catalog
                :: pageRoot allPages_ pdf_ usedFonts fontOffset
                :: List.indexedMap (\index font -> fontObject { index = index + 4, revision = 0 } font) usedFonts
                ++ (dictToIndexDict (images pdf_)
                        |> Dict.toList
                        |> List.map
                            (\( _, { value, index } ) ->
                                imageObject (fontOffset + List.length usedFonts + index - 1) value
                            )
                   )
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


infoIndirectReference : IndirectReference_
infoIndirectReference =
    { index = 1, revision = 0 }


catalogIndirectReference : IndirectReference_
catalogIndirectReference =
    { index = 2, revision = 0 }


pageRootIndirectReference : IndirectReference_
pageRootIndirectReference =
    { index = 3, revision = 0 }


pageRoot :
    List { page : IndirectObject, content : IndirectObject }
    -> Pdf
    -> List Font
    -> Int
    -> IndirectObject
pageRoot allPages_ pdf_ fonts fontIndexOffset =
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
                (( "Font"
                 , List.indexedMap
                    (\index _ ->
                        ( "F" ++ String.fromInt (index + 1)
                        , IndirectReference { index = index + fontIndexOffset, revision = 0 }
                        )
                    )
                    fonts
                    |> PdfDict
                 )
                    :: ( "PRocSet", PdfArray [ Name "PDF", Name "Text" ] )
                    :: (case dictToIndexDict (images pdf_) |> Dict.toList of
                            head :: rest ->
                                head
                                    :: rest
                                    |> List.map
                                        (\( _, { index } ) ->
                                            ( "Im" ++ String.fromInt index
                                            , IndirectReference
                                                { index = index + List.length fonts + fontIndexOffset - 1
                                                , revision = 0
                                                }
                                            )
                                        )
                                    |> PdfDict
                                    |> Tuple.pair "XObject"
                                    |> List.singleton

                            [] ->
                                []
                       )
                )
           )
         ]
            |> PdfDict
        )


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


imageObject : Int -> JpgImage_ -> IndirectObject
imageObject index image =
    let
        ( width, height ) =
            Tuple.mapBoth Pixels.inPixels Pixels.inPixels image.size
    in
    IndirectObject
        { index = index
        , revision = 0
        , isEntryPoint = False
        , object =
            Stream
                [ ( "Type", Name "XObject" )
                , ( "Subtype", Name "Image" )
                , ( "Width", PdfInt width )
                , ( "Height", PdfInt height )
                , ( "ColorSpace", Name "DeviceRGB" )
                , ( "BitsPerComponent", PdfInt 8 )
                , ( "Filter", Name "DCTDecode" )
                ]
                (ResourceData image.jpgData)
        }


dictToIndexDict : Dict comparable a -> Dict comparable { index : Int, value : a }
dictToIndexDict =
    Dict.toList
        >> List.indexedMap (\index ( key, value ) -> ( key, { index = index + 1, value = value } ))
        >> Dict.fromList


pageObjects : List Font -> Dict ImageId JpgImage_ -> Int -> List Page -> List { page : IndirectObject, content : IndirectObject }
pageObjects fonts images_ indexStart pages_ =
    let
        fontLookup =
            List.indexedMap (\index font -> ( fontName font, index + 1 )) fonts
                |> Dict.fromList

        imageLookup : Dict ImageId Int
        imageLookup =
            dictToIndexDict images_ |> Dict.map (\_ { index } -> index)
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
                                            fontIndex =
                                                Dict.get (fontName text_.font) fontLookup |> Maybe.withDefault 1
                                        in
                                        drawText text_.fontSize fontIndex text_.text text_.position previous

                                    JpgImage image ->
                                        drawImage
                                            image.boundingBox
                                            (Dict.get image.imageId imageLookup |> Maybe.withDefault 1)
                                            previous
                            )
                            (initIntermediateInstructions pageSize)
                            pageText
                            |> endIntermediateInstructions
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


type alias IntermediateInstructions =
    { instructions : String
    , cursorPosition : Point2d Meters PageCoordinates
    , fontSize : Length
    , fontIndex : Int
    }


initIntermediateInstructions : Vector2d Meters coordinates -> IntermediateInstructions
initIntermediateInstructions pageSize =
    { instructions = ""
    , cursorPosition = Point2d.translateBy (Vector2d.xy Quantity.zero (Vector2d.yComponent pageSize)) Point2d.origin
    , fontSize = Length.points -1
    , fontIndex = -1
    }


endIntermediateInstructions : IntermediateInstructions -> String
endIntermediateInstructions intermediateInstructions =
    "BT " ++ intermediateInstructions.instructions ++ "ET"


drawImage : BoundingBox2d Meters PageCoordinates -> Int -> IntermediateInstructions -> IntermediateInstructions
drawImage bounds imageIndex intermediate =
    { intermediate | instructions = "/Im" ++ String.fromInt imageIndex ++ " Do " }


positionImage pageSize bounds =
    let
        ( width, height ) =
            BoundingBox2d.dimensions bounds

        { minX, minY } =
            BoundingBox2d.extrema bounds
    in
    [ minX, Quantity.zero, Quantity.zero, ( minY, width, Vector2d.yComponent pageSize height ]
        |> List.map lengthToString
        |> List.intersperse " "
        |> (\a -> a ++ " cm ")


drawText : Length -> Int -> String -> Point2d Meters PageCoordinates -> IntermediateInstructions -> IntermediateInstructions
drawText fontSize fontIndex text_ position intermediate =
    if text_ == "" then
        intermediate

    else
        let
            actualPosition =
                Point2d.translateBy (Vector2d.xy Quantity.zero fontSize) position

            lines =
                String.lines text_

            instructions : List (IntermediateInstructions -> IntermediateInstructions)
            instructions =
                List.map drawTextLine lines |> List.intersperse (moveCursor (Vector2d.xy Quantity.zero fontSize))
        in
        intermediate
            |> (if fontIndex /= intermediate.fontIndex || fontSize /= intermediate.fontSize then
                    setFont fontIndex fontSize

                else
                    identity
               )
            |> (if actualPosition /= intermediate.cursorPosition then
                    moveCursor (Vector2d.from intermediate.cursorPosition actualPosition)

                else
                    identity
               )
            |> (\intermediate_ -> List.foldl (\nextInstruction state -> nextInstruction state) intermediate_ instructions)


drawTextLine : String -> IntermediateInstructions -> IntermediateInstructions
drawTextLine line intermediate =
    { intermediate | instructions = intermediate.instructions ++ textToString line ++ " Tj " }


setFont : Int -> Length -> IntermediateInstructions -> IntermediateInstructions
setFont fontIndex fontSize intermediate =
    { intermediate
        | instructions = intermediate.instructions ++ "/F" ++ String.fromInt fontIndex ++ " " ++ lengthToString fontSize ++ " Tf "
    }


moveCursor : Vector2d Meters PageCoordinates -> IntermediateInstructions -> IntermediateInstructions
moveCursor offset intermediate =
    let
        ( x, y ) =
            Vector2d.toTuple Length.inPoints offset
    in
    { intermediate
        | instructions = intermediate.instructions ++ floatToString x ++ " " ++ floatToString -y ++ " Td "
        , cursorPosition = Point2d.translateBy offset intermediate.cursorPosition
    }


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
                                deflate =
                                    True

                                textBytes =
                                    text_
                                        |> BE.string
                                        |> BE.encode
                                        |> (if deflate then
                                                Flate.deflateZlib

                                            else
                                                identity
                                           )
                            in
                            ( textBytes
                            , ( "Length", PdfInt (Bytes.width textBytes) )
                                :: (if deflate then
                                        [ ( "Filter", Name "FlateDecode" ) ]

                                    else
                                        []
                                   )
                                ++ dict
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



--- Fonts ---


{-| -}
type Font
    = Courier { bold : Bool, oblique : Bool }
    | Helvetica { bold : Bool, oblique : Bool }
    | TimesRoman { bold : Bool, italic : Bool }
    | Symbol
    | ZapfDingbats


{-| Courier, a monospaced font.
-}
courier : { bold : Bool, oblique : Bool } -> Font
courier { bold, oblique } =
    Courier { bold = bold, oblique = oblique }


{-| Helvetica, a san-serif font.
-}
helvetica : { bold : Bool, oblique : Bool } -> Font
helvetica { bold, oblique } =
    Helvetica { bold = bold, oblique = oblique }


{-| Times Roman font, a serif font.
It's not the same as Times _New_ Roman but it's very similar looking.
-}
timesRoman : { bold : Bool, italic : Bool } -> Font
timesRoman { bold, italic } =
    TimesRoman { bold = bold, italic = italic }


{-| A font made up of a bunch of symbols.
-}
symbol : Font
symbol =
    Symbol


{-| Another font made up of a bunch of symbols.
-}
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
