module Pdf exposing
    ( pdf, page, paperSize, encoder, toBytes, Pdf, Page, ASizes(..), Orientation(..)
    , text, imageFit, imageStretch, Item, PageCoordinates
    , jpeg, imageSize, Image, ImageId
    , helvetica, timesRoman, courier, symbol, zapfDingbats, Font
    , DecodedPdf, Object(..), fromBytes, topLevelObjectParser
    )

{-| In order to use this package you'll need to install
[`ianmackenzie/elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/)
, [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)
, and [`elm/bytes`](https://package.elm-lang.org/packages/elm/bytes/latest/).


# PDF creation

@docs pdf, page, paperSize, encoder, toBytes, Pdf, Page, ASizes, Orientation


# Page content

The content to show on a page.
Currently only text and images can be shown and a lot of features are missing such as automatic line breaks, unicode, and custom fonts.

@docs text, imageFit, imageStretch, Item, PageCoordinates


# Image resources

Before you can add an image to a page you need to load it. Currently only jpeg images are supported.

Note that, the PDF standard only supports jpeg, jpeg2000 (a successor to jpeg that no one uses), and raw bitmap data.
The best this package can possibly do is automatically convert unsupported formats (PNG images, for example) into one of those.
For now it's up to the user to do that conversion.

@docs jpeg, imageSize, Image, ImageId


# Built-in fonts

There are a few fonts that PDF supports by default.
Custom fonts have to be embedded in the file in order to be used and this package doesn't support that yet.

@docs helvetica, timesRoman, courier, symbol, zapfDingbats, Font

-}

import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Flate
import Hex.Convert
import Length exposing (Length, Meters)
import List.Nonempty exposing (Nonempty(..))
import Parser exposing ((|.), (|=), Parser)
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
    = Pdf { title : String, pages : List Page }


{-| -}
type Page
    = Page (Vector2d Meters PageCoordinates) (List Item)


{-| Something that gets drawn to a page.
-}
type Item
    = TextItem
        { position : Point2d Meters PageCoordinates
        , font : Font
        , text : String
        , fontSize : Length
        }
    | ImageItem { boundingBox : ImageBounds, image : Image }


type ImageBounds
    = ImageStretch (BoundingBox2d Meters PageCoordinates)
    | ImageFit (BoundingBox2d Meters PageCoordinates)


{-| An image that we can draw onto some pages.
-}
type Image
    = JpegImage
        { imageId : String
        , size : ( Quantity Int Pixels, Quantity Int Pixels )
        , jpegData : Bytes
        }


imageId : Image -> ImageId
imageId (JpegImage image) =
    image.imageId


jpegSizeDecoder : BD.Decoder ( Quantity Int Pixels, Quantity Int Pixels )
jpegSizeDecoder =
    BD.map2 (\a b -> a == 0xFF && b == 0xD8)
        BD.unsignedInt8
        BD.unsignedInt8
        |> BD.andThen
            (\validHeader ->
                if validHeader then
                    BD.loop ()
                        (\() ->
                            BD.andThen
                                (\value ->
                                    if value == 0xFF then
                                        BD.andThen
                                            (\value_ ->
                                                if value_ == 0xC0 || value_ == 0xC2 then
                                                    BD.succeed (BD.Done ())

                                                else
                                                    BD.succeed (BD.Loop ())
                                            )
                                            BD.unsignedInt8

                                    else
                                        BD.succeed (BD.Loop ())
                                )
                                BD.unsignedInt8
                        )

                else
                    BD.fail
            )
        |> BD.andThen
            (\_ ->
                BD.map3 (\_ height width -> ( Pixels.pixels width, Pixels.pixels height ))
                    (BD.bytes 3)
                    (BD.unsignedInt16 Bytes.BE)
                    (BD.unsignedInt16 Bytes.BE)
            )


{-| Get the pixel dimensions of an image.
-}
imageSize : Image -> ( Quantity Int Pixels, Quantity Int Pixels )
imageSize image =
    case image of
        JpegImage { size } ->
            size


{-| Create a jpeg image. The string provided is an identifier. Make sure it's unique for each image.
-}
jpeg : ImageId -> Bytes -> Maybe Image
jpeg imageId_ bytes =
    case BD.decode jpegSizeDecoder bytes of
        Just size ->
            { imageId = imageId_
            , size = size
            , jpegData = bytes
            }
                |> JpegImage
                |> Just

        Nothing ->
            Nothing



--{-| Same as [`jpeg`](#jpeg) but you provide the image dimensions yourself.
--This is less safe but might be necessary if you have a large jpeg that's slow to parse.
---}
--unsafeJpeg : ImageId -> Bytes -> ( Quantity Int Pixels, Quantity Int Pixels ) -> Image
--unsafeJpeg imageId_ bytes size =
--    JpegImage
--        { imageId = imageId_
--        , size = size
--        , jpegData = bytes
--        }


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


{-| Draw an image with its width and height distorted to fill a bounding box.
-}
imageStretch : BoundingBox2d Meters PageCoordinates -> Image -> Item
imageStretch bounds image =
    ImageItem
        { boundingBox = ImageStretch bounds
        , image = image
        }


{-| Fit image inside a bounding box while maintaining aspect ratio.
-}
imageFit : BoundingBox2d Meters PageCoordinates -> Image -> Item
imageFit bounds image =
    ImageItem
        { boundingBox = ImageFit bounds
        , image = image
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


{-| The orientation of our page.
Landscape means the long edge of the page is horizontal and portrait means the long edge of the page is vertical.
-}
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
-}
pdf : { title : String, pages : List Page } -> Pdf
pdf =
    Pdf


title : Pdf -> String
title (Pdf pdf_) =
    pdf_.title


pages : Pdf -> List Page
pages (Pdf pdf_) =
    pdf_.pages


images : Pdf -> Dict ImageId Image
images =
    pages
        >> List.concatMap
            (\(Page _ items) ->
                items
                    |> List.filterMap
                        (\item ->
                            case item of
                                ImageItem { image } ->
                                    Just ( imageId image, image )

                                TextItem _ ->
                                    Nothing
                        )
            )
        >> Dict.fromList



--- ENCODE ---


{-| Convert PDF to binary data that can be used as a PDF file.

This is the same as:

    import Bytes.Encode
    import Pdf

    Pdf.encoder myPdf |> Bytes.Encode.encode

-}
toBytes : Pdf -> Bytes
toBytes =
    encoder >> BE.encode


decodeInt : Char -> BD.Decoder (Result String Int)
decodeInt delimiter =
    decodeAndThen
        decodeUtf8Char
        (\char ->
            if Char.isDigit char then
                decodeIntHelper delimiter char

            else if char == '-' then
                decodeIntHelper delimiter char

            else
                BD.succeed (Err "Not an int")
        )


decodeIntHelper : Char -> Char -> BD.Decoder (Result String Int)
decodeIntHelper delimiter firstChar =
    BD.loop
        [ firstChar ]
        (\list ->
            decodeUtf8Char
                |> BD.andThen
                    (\result ->
                        case result of
                            Ok char ->
                                if char == delimiter then
                                    case List.reverse list |> String.fromList |> String.toInt of
                                        Just int ->
                                            BD.Done (Ok int) |> BD.succeed

                                        Nothing ->
                                            Err "Not an int" |> BD.Done |> BD.succeed

                                else if Char.isDigit char then
                                    char :: list |> BD.Loop |> BD.succeed

                                else
                                    "Expected "
                                        ++ String.fromChar delimiter
                                        ++ " or digit when parsing int, instead got "
                                        ++ String.fromChar char
                                        |> Err
                                        |> BD.Done
                                        |> BD.succeed

                            Err error ->
                                Err error |> BD.Done |> BD.succeed
                    )
        )


decodeUtf8Char : BD.Decoder (Result String Char)
decodeUtf8Char =
    BD.unsignedInt8
        |> BD.andThen
            (\value ->
                if value < 128 then
                    Char.fromCode value |> Ok |> BD.succeed

                else
                    Err "Invalid UTF8 char" |> BD.succeed
            )


decodeSymbol : String -> BD.Decoder (Result String ())
decodeSymbol text2 =
    BD.string (BE.string text2 |> BE.encode |> Bytes.width)
        |> BD.andThen
            (\text3 ->
                if text2 == text3 then
                    BD.succeed (Ok ())

                else
                    BD.succeed (Err ("Expected " ++ text2 ++ " but got " ++ text3))
            )


decodeAndThen : BD.Decoder (Result String b) -> (b -> BD.Decoder (Result String a)) -> BD.Decoder (Result String a)
decodeAndThen previous func =
    BD.andThen
        (\result ->
            case result of
                Ok a ->
                    func a

                Err error ->
                    BD.succeed (Err error)
        )
        previous


decodeHeader : BD.Decoder (Result String PdfVersion)
decodeHeader =
    decodeAndThen
        (decodeSymbol "%PDF-")
        (\() ->
            decodeAndThen
                (decodeInt '.')
                (\majorVersion ->
                    decodeAndThen
                        (decodeInt '\n')
                        (\minorVersion ->
                            Ok { major = majorVersion, minor = minorVersion }
                                |> BD.succeed
                        )
                )
        )


type alias DecodedPdf =
    { xRef : XRefTable, sections : List ( IndirectReference_, List ( String, Object ) ) }


type alias PdfVersion =
    { major : Int, minor : Int }


getXRefOffset : Bytes -> Result String Int
getXRefOffset bytes =
    BD.decode
        (BD.map2
            (\_ eofText ->
                case String.indexes "startxref\n" eofText of
                    [ index ] ->
                        case String.dropLeft index eofText |> String.split "\n" of
                            [ _, offsetText, "%%EOF" ] ->
                                case String.toInt offsetText of
                                    Just int ->
                                        Ok int

                                    Nothing ->
                                        Err ("Unexpected EOF text " ++ eofText)

                            _ ->
                                Err ("Unexpected EOF text " ++ eofText)

                    _ ->
                        Err ("Unexpected EOF text " ++ eofText)
            )
            (BD.bytes (Bytes.width bytes - 30))
            (BD.string 30)
        )
        bytes
        |> (\maybeResult ->
                case maybeResult of
                    Just result ->
                        result

                    Nothing ->
                        Err "Parsing failed"
           )


type alias XRefTable =
    { references : List Int, metadata : List ( String, Object ) }


fromBytes : Bytes -> Result String DecodedPdf
fromBytes bytes =
    case getXRefOffset bytes of
        Ok xRefOffset ->
            let
                xrefDecoder =
                    BD.map2
                        (\_ endSection -> Parser.run xRefParser endSection |> Debug.log "xref")
                        (BD.bytes xRefOffset)
                        (BD.string (Bytes.width bytes - xRefOffset))
            in
            case BD.decode xrefDecoder bytes of
                Just result ->
                    case result of
                        Ok xRef ->
                            let
                                sections =
                                    List.foldr
                                        (\offset state ->
                                            let
                                                size =
                                                    state.next - offset

                                                result2 =
                                                    case sliceBytes offset size bytes of
                                                        Just endSection ->
                                                            let
                                                                _ =
                                                                    Debug.log "abc" endSectionText

                                                                endSectionText =
                                                                    BD.decode (BD.string size) endSection |> Maybe.withDefault ""
                                                            in
                                                            Parser.run
                                                                (topLevelObjectParser endSection endSectionText)
                                                                endSectionText
                                                                |> Debug.log "section"

                                                        Nothing ->
                                                            Err []
                                            in
                                            { next = offset
                                            , sections =
                                                case result2 of
                                                    Ok ok ->
                                                        ok :: state.sections

                                                    Err error ->
                                                        state.sections
                                            }
                                        )
                                        { next = xRefOffset, sections = [] }
                                        xRef.references
                                        |> .sections
                            in
                            Ok { xRef = xRef, sections = sections }

                        Err _ ->
                            Err "Failed to decode xref"

                Nothing ->
                    Err "Failed to decode xref"

        Err error ->
            Err error


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
            indirectObject
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

                                    ImageItem _ ->
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

        encodeXRef : XRef -> BE.Encoder
        encodeXRef (XRef xRefs) =
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
                , encodePdfDict
                    [ ( "Size", PdfInt xRefCount )
                    , ( "Info", IndirectReference infoIndirectReference )
                    , ( "Root", IndirectReference catalogIndirectReference )
                    ]
                , BE.string "\nstartxref\n"
                ]
    in
    BE.sequence
        [ BE.bytes content
        , encodeXRef (XRef xRef)
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
                |> Array.fromList
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
                    :: ( "PRocSet", [ Name "PDF", Name "Text" ] |> Array.fromList |> PdfArray )
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


imageObject : Int -> Image -> IndirectObject
imageObject index (JpegImage image) =
    let
        ( width, height ) =
            Tuple.mapBoth Pixels.inPixels Pixels.inPixels image.size
    in
    IndirectObject
        { index = index
        , revision = 0
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
                (ResourceData image.jpegData)
        }


dictToIndexDict : Dict comparable a -> Dict comparable { index : Int, value : a }
dictToIndexDict =
    Dict.toList
        >> List.indexedMap (\index ( key, value ) -> ( key, { index = index + 1, value = value } ))
        >> Dict.fromList


pageObjects :
    List Font
    -> Dict ImageId Image
    -> Int
    -> List Page
    -> List { page : IndirectObject, content : IndirectObject }
pageObjects fonts images_ indexStart pages_ =
    let
        fontLookup =
            List.indexedMap (\index font -> ( fontName font, index + 1 )) fonts
                |> Dict.fromList

        imageLookup : Dict ImageId { index : Int, value : Image }
        imageLookup =
            dictToIndexDict images_
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

                                    ImageItem image_ ->
                                        case Dict.get (imageId image_.image) imageLookup of
                                            Nothing ->
                                                previous

                                            Just image ->
                                                let
                                                    (JpegImage imageData) =
                                                        image.value
                                                in
                                                case adjustBoundingBox image_ imageData.size of
                                                    Just bounds ->
                                                        drawImage
                                                            bounds
                                                            image.index
                                                            previous

                                                    Nothing ->
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


adjustBoundingBox :
    { a | boundingBox : ImageBounds }
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Maybe (BoundingBox2d Meters PageCoordinates)
adjustBoundingBox image_ ( w, h ) =
    case image_.boundingBox of
        ImageStretch boundingBox ->
            Just boundingBox

        ImageFit boundingBox ->
            let
                imageAspectRatio =
                    Quantity.ratio
                        (Quantity.toFloatQuantity w)
                        (Quantity.toFloatQuantity h)

                ( bw, bh ) =
                    BoundingBox2d.dimensions boundingBox

                aspectRatio =
                    Quantity.ratio bw bh

                aspectRatioDiff =
                    aspectRatio / imageAspectRatio

                aspectRatioDiffInv =
                    imageAspectRatio / aspectRatio
            in
            if
                List.any
                    (\a -> isNaN a || isInfinite a)
                    [ aspectRatio, imageAspectRatio, aspectRatioDiff, aspectRatioDiffInv ]
            then
                Nothing

            else if imageAspectRatio < aspectRatio then
                boundingBoxScaleX aspectRatioDiffInv (BoundingBox2d.midX boundingBox) boundingBox |> Just

            else
                boundingBoxScaleY aspectRatioDiff (BoundingBox2d.midY boundingBox) boundingBox |> Just


boundingBoxScaleX : Float -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
boundingBoxScaleX scaleBy xPosition bounds =
    BoundingBox2d.fromExtrema
        { minX =
            BoundingBox2d.minX bounds
                |> Quantity.minus xPosition
                |> Quantity.multiplyBy scaleBy
                |> Quantity.plus xPosition
        , maxX =
            BoundingBox2d.maxX bounds
                |> Quantity.minus xPosition
                |> Quantity.multiplyBy scaleBy
                |> Quantity.plus xPosition
        , minY = BoundingBox2d.minY bounds
        , maxY = BoundingBox2d.maxY bounds
        }


boundingBoxScaleY : Float -> Quantity Float units -> BoundingBox2d units coordinates -> BoundingBox2d units coordinates
boundingBoxScaleY scaleBy yPosition bounds =
    BoundingBox2d.fromExtrema
        { minY =
            BoundingBox2d.minY bounds
                |> Quantity.minus yPosition
                |> Quantity.multiplyBy scaleBy
                |> Quantity.plus yPosition
        , maxY =
            BoundingBox2d.maxY bounds
                |> Quantity.minus yPosition
                |> Quantity.multiplyBy scaleBy
                |> Quantity.plus yPosition
        , minX = BoundingBox2d.minX bounds
        , maxX = BoundingBox2d.maxX bounds
        }


type alias IntermediateInstructions =
    { instructions : String
    , cursorPosition : Point2d Meters PageCoordinates
    , fontSize : Length
    , pageSize : Vector2d Meters PageCoordinates
    , fontIndex : Int
    }


initIntermediateInstructions : Vector2d Meters PageCoordinates -> IntermediateInstructions
initIntermediateInstructions pageSize =
    { instructions = ""
    , cursorPosition = pageCoordToPdfCoord pageSize Point2d.origin
    , pageSize = pageSize
    , fontSize = Length.points -1
    , fontIndex = -1
    }


endIntermediateInstructions : IntermediateInstructions -> String
endIntermediateInstructions intermediateInstructions =
    "BT " ++ intermediateInstructions.instructions ++ "ET"


drawImage : BoundingBox2d Meters PageCoordinates -> Int -> IntermediateInstructions -> IntermediateInstructions
drawImage bounds imageIndex intermediate =
    let
        ( width, height ) =
            BoundingBox2d.dimensions bounds

        { minX, maxY } =
            BoundingBox2d.extrema bounds

        ( x, y ) =
            Point2d.xy minX maxY |> pageCoordToPdfCoord intermediate.pageSize |> Point2d.toTuple Length.inPoints

        position =
            [ Length.inPoints width, 0, 0, Length.inPoints height, x, y ]
                |> List.map floatToString
                |> String.join " "
                |> (\a -> "q " ++ a ++ " cm ")
    in
    { intermediate
        | instructions =
            intermediate.instructions ++ position ++ "/Im" ++ String.fromInt imageIndex ++ " Do Q "
    }


pageCoordToPdfCoord : Vector2d Meters PageCoordinates -> Point2d Meters PageCoordinates -> Point2d Meters a
pageCoordToPdfCoord pageSize coord =
    Point2d.xy (Point2d.xCoordinate coord) (Vector2d.yComponent pageSize |> Quantity.minus (Point2d.yCoordinate coord))


drawText :
    Length
    -> Int
    -> String
    -> Point2d Meters PageCoordinates
    -> IntermediateInstructions
    -> IntermediateInstructions
drawText fontSize fontIndex text_ position intermediate =
    if String.isEmpty text_ then
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
        | instructions =
            intermediate.instructions
                ++ "/F"
                ++ String.fromInt fontIndex
                ++ " "
                ++ lengthToString fontSize
                ++ " Tf "
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
                ( BE.sequence [ BE.bytes content_, encodeIndirectObject indirectObject_, BE.string "\n" ] |> BE.encode
                , { offset = Bytes.width content_ } :: xRef_
                , index + 1
                )
            )
            ( header
            , []
            , 1
            )
        >> (\( content, xRef, _ ) -> ( content, List.reverse xRef ))


header : Bytes
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


pdfVersion : String
pdfVersion =
    "1.7"


mediaBox : Vector2d Meters PageCoordinates -> ( String, Object )
mediaBox size =
    let
        ( w, h ) =
            Vector2d.toTuple Length.inPoints size
    in
    ( "MediaBox", [ PdfInt 0, PdfInt 0, PdfFloat w, PdfFloat h ] |> Array.fromList |> PdfArray )


type XRef
    = XRef (List { offset : Int })


type Object
    = Name String
    | PdfFloat Float
    | PdfInt Int
    | PdfDict (List ( String, Object ))
    | PdfArray (Array Object)
    | Text String
    | Stream (List ( String, Object )) StreamContent
    | IndirectReference IndirectReference_


encodeObject : Object -> BE.Encoder
encodeObject object =
    case object of
        Name name ->
            nameToString name |> BE.string

        PdfFloat float ->
            floatToString float |> BE.string

        PdfInt int ->
            String.fromInt int |> BE.string

        PdfDict pdfDict ->
            encodePdfDict pdfDict

        PdfArray pdfArray ->
            let
                contentText =
                    Array.toList pdfArray |> List.map encodeObject |> List.intersperse (BE.string " ")
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
                                    False

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
                [ encodePdfDict dict2
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


encodePdfDict : List ( String, Object ) -> BE.Encoder
encodePdfDict =
    List.map (\( key, value ) -> BE.sequence [ BE.string (nameToString key ++ " "), encodeObject value ])
        >> List.intersperse (BE.string " ")
        >> (\a -> BE.string "<< " :: a ++ [ BE.string " >>" ])
        >> BE.sequence


xRefParser : Parser XRefTable
xRefParser =
    Parser.succeed (\list metadata -> { references = list, metadata = metadata })
        |. Parser.symbol "xref"
        |. Parser.spaces
        |. Parser.int
        |. Parser.spaces
        |. Parser.int
        |. Parser.spaces
        |= xRefTableParser
        |. Parser.symbol "trailer"
        |. Parser.spaces
        |= dictParser
        |. Parser.spaces
        |. Parser.symbol "startxref"


paddingIntParser : Parser Int
paddingIntParser =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\string ->
                case String.toInt string of
                    Just ok ->
                        Parser.succeed ok

                    Nothing ->
                        Parser.problem "Not a valid int"
            )


xRefTableParser : Parser (List Int)
xRefTableParser =
    Parser.succeed (\list -> list)
        |. xRefEntryParser
        |= Parser.loop
            []
            (\list ->
                Parser.oneOf
                    [ xRefEntryParser |> Parser.map (\a -> a :: list |> Parser.Loop)
                    , List.reverse list |> Parser.Done |> Parser.succeed
                    ]
            )


xRefEntryParser : Parser Int
xRefEntryParser =
    Parser.succeed identity
        |= paddingIntParser
        |. Parser.spaces
        |. paddingIntParser
        |. Parser.spaces
        |. Parser.chompIf (\char -> char == 'n' || char == 'f')
        |. Parser.spaces


dictParser : Parser (List ( String, Object ))
dictParser =
    Parser.succeed identity
        |. Parser.symbol "<<"
        |. Parser.spaces
        |= Parser.loop
            []
            (\list ->
                Parser.oneOf
                    [ Parser.symbol ">>" |> Parser.map (\() -> List.reverse list |> Parser.Done)
                    , Parser.succeed (\key value -> ( key, value ) :: list |> Parser.Loop)
                        |= nameParser
                        |. Parser.spaces
                        |= objectParser
                        |. Parser.spaces
                    ]
            )
        |. Parser.spaces


streamParser : Bytes -> String -> List ( String, Object ) -> Parser ( List ( String, Object ), Maybe String )
streamParser originalBytes originalText dict =
    case
        ( List.filterMap
            (\( key, value ) ->
                case ( key, value ) of
                    ( "Length", PdfInt length ) ->
                        Just length

                    _ ->
                        Nothing
            )
            dict
        , List.filterMap
            (\( key, value ) ->
                case ( key, value ) of
                    ( "Filter", Name filterName ) ->
                        Just filterName

                    _ ->
                        Nothing
            )
            dict
            |> List.head
        )
    of
        ( [ length ], maybeFilter ) ->
            Parser.succeed
                (\( row, column ) ->
                    let
                        offset =
                            String.split "\n" originalText
                                |> List.take (row - 1)
                                |> List.map String.length
                                |> List.sum
                                |> (+) column
                                |> Debug.log "offset"

                        _ =
                            Debug.log "position" ( row, column )

                        result : String
                        result =
                            case sliceBytes offset length originalBytes of
                                Just bytes ->
                                    let
                                        _ =
                                            Debug.log "bytesToString" (Hex.Convert.toString bytes)
                                    in
                                    case maybeFilter of
                                        Just "FlateDecode" ->
                                            case Flate.inflateZlib bytes of
                                                Just inflated ->
                                                    BD.decode (BD.string (Bytes.width inflated)) inflated
                                                        |> Maybe.withDefault "to string failed"

                                                Nothing ->
                                                    "Inflate failed"

                                        Just filter ->
                                            "Can't handle that filter type: " ++ filter

                                        Nothing ->
                                            BD.decode (BD.string (Bytes.width bytes)) bytes
                                                |> Maybe.withDefault "to string failed"

                                Nothing ->
                                    "Failed to slice bytes"

                        _ =
                            Debug.log "stream" result
                    in
                    ( dict, Nothing )
                )
                |. Parser.symbol "stream\n"
                |= Parser.getPosition

        ( [], _ ) ->
            Parser.succeed ( dict, Nothing )
                |. Parser.symbol "endobj"

        _ ->
            Parser.problem "Found multiple length values"


topLevelObjectParser : Bytes -> String -> Parser ( IndirectReference_, List ( String, Object ) )
topLevelObjectParser originalBytes originalText =
    Parser.succeed (\index revision ( dict, maybeStream ) -> ( { index = index, revision = revision }, dict ))
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "obj"
        |= (dictParser |> Parser.andThen (streamParser originalBytes originalText))


sliceBytes : Int -> Int -> Bytes -> Maybe Bytes
sliceBytes offset size bytes =
    BD.decode (BD.map2 (\_ a -> a) (BD.bytes offset) (BD.bytes size)) bytes


basicObjectParser : Parser Object
basicObjectParser =
    Parser.oneOf
        [ dictParser |> Parser.map PdfDict
        , nameParser |> Parser.map Name
        , textParser
        , arrayParser
        ]


objectParser : Parser Object
objectParser =
    Parser.oneOf
        [ intFloatOrIndirectReferenceParser
            |> Parser.andThen
                (\list ->
                    case list of
                        [ single ] ->
                            Parser.succeed single

                        _ ->
                            Parser.problem "Wrong number of objects"
                )
        , basicObjectParser
        ]
        |. Parser.spaces


intFloatOrIndirectReferenceParser : Parser (List Object)
intFloatOrIndirectReferenceParser =
    Parser.oneOf
        [ Parser.succeed (\int -> [ PdfInt -int ])
            |. Parser.symbol "-"
            |= Parser.int
        , backtrackableIntParser
            |. Parser.spaces
            |> Parser.andThen
                (\first ->
                    Parser.oneOf
                        [ backtrackableIntParser
                            |. Parser.spaces
                            |> Parser.andThen
                                (\second ->
                                    Parser.oneOf
                                        [ Parser.succeed [ IndirectReference { index = first, revision = second } ]
                                            |. Parser.symbol "R"
                                        , Parser.succeed (\third -> [ PdfInt first, PdfInt second, PdfInt third ])
                                            |= backtrackableIntParser
                                        , Parser.succeed (\third -> [ PdfInt first, PdfInt second, PdfFloat third ])
                                            |= Parser.float
                                        ]
                                )
                        , Parser.succeed (\second -> [ PdfInt first, PdfFloat second ])
                            |= Parser.float
                        , Parser.chompWhile
                            (\char -> char /= '/' && char /= '>' && char /= ']' && char /= ')' && char /= '}')
                            |> Parser.getChompedString
                            |> Parser.andThen
                                (\text2 ->
                                    if text2 == "" then
                                        Parser.succeed [ PdfInt first ]

                                    else
                                        Parser.problem "Failed to decode int, float, or indirect reference"
                                )
                        ]
                )
        , Parser.float |> Parser.map (\a -> [ PdfFloat a ])
        ]
        |. Parser.spaces


backtrackableIntParser : Parser Int
backtrackableIntParser =
    Parser.backtrackable Parser.int


objectInArrayParser : Parser (List Object)
objectInArrayParser =
    Parser.oneOf
        [ intFloatOrIndirectReferenceParser
        , basicObjectParser |> Parser.map List.singleton
        ]
        |. Parser.spaces


textParser : Parser Object
textParser =
    Parser.oneOf
        [ Parser.succeed Text
            |. Parser.symbol "<"
            |= (Parser.chompWhile (\char -> char /= '>') |> Parser.getChompedString)
            |. Parser.symbol ">"
        , Parser.succeed Text
            |. Parser.symbol "("
            |= (Parser.chompWhile (\char -> char /= ')') |> Parser.getChompedString)
            |. Parser.symbol ")"
        ]


arrayParser : Parser Object
arrayParser =
    Parser.succeed PdfArray
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.loop
            Array.empty
            (\array ->
                Parser.oneOf
                    [ Parser.succeed (\items -> Array.append array (Array.fromList items) |> Parser.Loop)
                        |= objectInArrayParser
                    , Parser.symbol "]" |> Parser.map (\() -> Parser.Done array)
                    ]
            )


nameParser : Parser String
nameParser =
    Parser.succeed identity
        |. Parser.symbol "/"
        |= (Parser.chompWhile
                (\char ->
                    (Char.toCode char >= 0x21)
                        && (Char.toCode char <= 0x7E)
                        && (char /= '%')
                        && (char /= '(')
                        && (char /= ')')
                        && (char /= '<')
                        && (char /= '>')
                        && (char /= '[')
                        && (char /= ']')
                        && (char /= '{')
                        && (char /= '}')
                        && (char /= '/')
                        && (char /= '#')
                )
                |> Parser.getChompedString
           )


type alias IndirectReference_ =
    { index : Int, revision : Int }


type IndirectObject
    = IndirectObject { index : Int, revision : Int, object : Object }


indirectObject : IndirectReference_ -> Object -> IndirectObject
indirectObject { index, revision } object =
    IndirectObject { index = index, revision = revision, object = object }


indirectObjectToIndirectReference : IndirectObject -> IndirectReference_
indirectObjectToIndirectReference (IndirectObject { index, revision }) =
    { index = index, revision = revision }


encodeIndirectObject : IndirectObject -> BE.Encoder
encodeIndirectObject (IndirectObject { index, revision, object }) =
    BE.sequence
        [ String.fromInt index
            ++ " "
            ++ String.fromInt revision
            ++ " obj"
            |> BE.string
        , encodeObject object
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
