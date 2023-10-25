module Pdf exposing
    ( pdf, page, paperSize, encoder, toBytes, Pdf, Page, ASizes(..), Orientation(..)
    , text, imageFit, imageStretch, Item, PageCoordinates
    , jpeg, imageSize, Image, ImageId
    , helvetica, timesRoman, courier, symbol, zapfDingbats, Font
    , DecodedPdf, GraphicsInstruction, Object(..), Operator(..), StreamContent(..), bytesToInts, decodeAscii, decryptStream, emptyBytes, encodeAscii, fromBytes, getRc4Key, streamContentParser, topLevelObjectParser
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
import Bitwise
import BoundingBox2d exposing (BoundingBox2d)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Flate
import Hex.Convert
import Length exposing (Length, Meters)
import Md5
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rc4_2
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
    { metadata : Dict String Object
    , pages : List (Result (List DeadEnd) (List String))
    }


type alias PdfVersion =
    { major : Int, minor : Int }


getXRefOffset : Bytes -> Result String Int
getXRefOffset bytes =
    BD.decode
        (BD.map2
            (\_ eofText ->
                case String.indexes "startxref" eofText of
                    [ index ] ->
                        case String.dropLeft index eofText |> String.trim |> String.split "\n" of
                            [ _, offsetText, _ ] ->
                                case String.toInt (String.trim offsetText) of
                                    Just int ->
                                        Ok int

                                    Nothing ->
                                        Err ("Unexpected EOF text 3: " ++ eofText)

                            _ ->
                                Err ("Unexpected EOF text 2: " ++ eofText)

                    _ ->
                        Err ("Unexpected EOF text 1: " ++ eofText)
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
    { references : List Int, metadata : Dict String Object }


fromBytes : Bytes -> Result String DecodedPdf
fromBytes bytes =
    case getXRefOffset bytes of
        Ok xRefOffset ->
            case sliceBytes xRefOffset (Bytes.width bytes - xRefOffset) bytes of
                Just bytes2 ->
                    case decodeAscii bytes2 |> Parser.run xRefParser of
                        Ok xRef ->
                            let
                                sections0 : Dict ( Int, Int ) Section
                                sections0 =
                                    List.foldr
                                        (\offset state ->
                                            let
                                                size =
                                                    state.next - offset
                                            in
                                            { next = offset
                                            , sections =
                                                case sliceBytes offset size bytes of
                                                    Just endSection ->
                                                        let
                                                            endSectionText =
                                                                decodeAscii endSection
                                                        in
                                                        case Parser.run topLevelReferenceParser endSectionText of
                                                            Ok ref ->
                                                                Dict.insert
                                                                    ( ref.index, ref.revision )
                                                                    (IsNotParsed { bytes = endSection, text = endSectionText })
                                                                    state.sections

                                                            Err error ->
                                                                state.sections

                                                    Nothing ->
                                                        state.sections
                                            }
                                        )
                                        { next = xRefOffset, sections = Dict.empty }
                                        (List.sort xRef.references)
                                        |> .sections

                                ( sections1, maybeEncryption ) =
                                    handleEncryption xRef sections0
                            in
                            case Dict.get "Root" xRef.metadata of
                                Just (IndirectReference ref) ->
                                    case parseSection2 maybeEncryption ref sections1 of
                                        ( sections2, Ok (PdfDict rootSection) ) ->
                                            case Dict.get "Pages" rootSection of
                                                Just (IndirectReference pagesRef) ->
                                                    case parseSection2 maybeEncryption pagesRef sections2 of
                                                        ( sections3, Ok (PdfDict pagesSection) ) ->
                                                            case Dict.get "Kids" pagesSection of
                                                                Just (PdfArray array) ->
                                                                    { metadata = xRef.metadata
                                                                    , pages =
                                                                        List.foldl
                                                                            (\pageRef ( sections4, parsedPages ) ->
                                                                                let
                                                                                    ( sections5, parsedPage ) =
                                                                                        getPageContents
                                                                                            maybeEncryption
                                                                                            sections4
                                                                                            pageRef
                                                                                in
                                                                                ( sections5, parsedPage :: parsedPages )
                                                                            )
                                                                            ( sections3, [] )
                                                                            (Array.toList array)
                                                                            |> Tuple.second
                                                                            |> List.reverse
                                                                    }
                                                                        |> Ok

                                                                _ ->
                                                                    Err "Missing page list"

                                                        ( sections3, _ ) ->
                                                            Err "Failed to parse pages object"

                                                _ ->
                                                    Err "Missing pages reference"

                                        ( sections2, _ ) ->
                                            Err "Failed to parse root object"

                                _ ->
                                    Err "Missing root reference"

                        Err error ->
                            Err "Failed to decode xref 2"

                Nothing ->
                    Err "Failed to decode xref"

        Err error ->
            Err error


type Section
    = IsParsed (Result (List DeadEnd) Object)
    | IsNotParsed { bytes : Bytes, text : String }


getFonts :
    Config
    -> Dict ( Int, Int ) Section
    -> Dict String Object
    -> ( Dict ( Int, Int ) Section, List (Dict String Char) )
getFonts config sections resourceDict =
    List.foldl
        (\( _, value ) ( sections5, fonts2 ) ->
            case value of
                IndirectReference fontRef ->
                    case parseSection2 config fontRef sections5 of
                        ( sections6, Ok (PdfDict fontDict) ) ->
                            case Dict.get "ToUnicode" fontDict of
                                Just (IndirectReference unicodeRef) ->
                                    case parseSection2 config unicodeRef sections6 of
                                        ( sections7, Ok (Stream _ (ToUnicodeData unicodeData)) ) ->
                                            ( sections7
                                            , unicodeData :: fonts2
                                            )

                                        ( sections7, a ) ->
                                            let
                                                _ =
                                                    Debug.log "aa" a
                                            in
                                            ( sections7, fonts2 )

                                _ ->
                                    let
                                        _ =
                                            Debug.log "b" ()
                                    in
                                    ( sections6, fonts2 )

                        ( sections6, _ ) ->
                            let
                                _ =
                                    Debug.log "c" ()
                            in
                            ( sections6, fonts2 )

                _ ->
                    let
                        _ =
                            Debug.log "d" ()
                    in
                    ( sections5, fonts2 )
        )
        ( sections, [] )
        (Dict.toList resourceDict)


type alias Config =
    { encryption : Maybe Bytes
    }


hexStringToString : Dict String Char -> String -> String
hexStringToString font hexString =
    (String.length hexString // 4 - 1)
        |> List.range 0
        |> List.map
            (\index ->
                let
                    slice : String
                    slice =
                        String.slice (index * 4) ((index + 1) * 4) hexString
                in
                case Dict.get slice font of
                    Just char ->
                        char

                    Nothing ->
                        '?'
            )
        |> String.fromList


getValue : comparable -> Config -> Dict comparable Object -> Dict ( Int, Int ) Section -> ( Dict ( Int, Int ) Section, Result (List DeadEnd) Object )
getValue key config dict sections =
    case Dict.get key dict of
        Just (IndirectReference resourcesRef) ->
            case parseSection2 config resourcesRef sections of
                ( sections2, result ) ->
                    ( sections2, result )

        Just object ->
            ( sections, Ok object )

        Nothing ->
            ( sections
            , Err
                [ { row = 0
                  , col = 0
                  , problem = Parser.Expecting "Key not found"
                  }
                ]
            )


getPageContents :
    Config
    -> Dict ( Int, Int ) Section
    -> Object
    -> ( Dict ( Int, Int ) Section, Result (List DeadEnd) (List String) )
getPageContents config sections refObject =
    case refObject of
        IndirectReference ref ->
            case parseSection2 config ref sections of
                ( sections2, Ok (PdfDict pageSection) ) ->
                    case ( Dict.get "Contents" pageSection, getValue "Resources" config pageSection sections2 ) of
                        ( Just (IndirectReference contentRef), ( sections3_2, Ok (PdfDict resources) ) ) ->
                            let
                                ( sections3, font ) =
                                    case Dict.get "Font" resources of
                                        Just (PdfDict fontDict) ->
                                            let
                                                ( sections4, fonts ) =
                                                    getFonts config sections3_2 fontDict
                                            in
                                            ( sections4
                                            , List.foldl (\value dict -> Dict.union value dict) Dict.empty fonts
                                            )

                                        _ ->
                                            ( sections3_2, Dict.empty )
                            in
                            case parseSection2 config contentRef sections3 of
                                ( sections4, Ok (Stream _ (DrawingInstructions drawingInstructions)) ) ->
                                    let
                                        mergeOrAppend :
                                            String
                                            -> { text : Array String, canMerge : Bool }
                                            -> { text : Array String, canMerge : Bool }
                                        mergeOrAppend text2 state =
                                            { text =
                                                case
                                                    ( Array.get (Array.length state.text - 1) state.text
                                                    , state.canMerge
                                                    )
                                                of
                                                    ( Just last, True ) ->
                                                        Array.set (Array.length state.text - 1) (last ++ text2) state.text

                                                    _ ->
                                                        Array.push text2 state.text
                                            , canMerge = True
                                            }
                                    in
                                    List.foldl
                                        (\instruction state ->
                                            case ( instruction.operator, instruction.parameters ) of
                                                ( Td, [ PdfFloat xOffset, PdfInt 0 ] ) ->
                                                    if abs xOffset < 20 then
                                                        state

                                                    else
                                                        { text = state.text, canMerge = False }

                                                ( Td, [ PdfInt xOffset, PdfInt 0 ] ) ->
                                                    if abs xOffset < 20 then
                                                        state

                                                    else
                                                        { text = state.text, canMerge = False }

                                                ( Tj, [ Text text2 ] ) ->
                                                    mergeOrAppend text2 state

                                                ( Tj, [ HexString text2 ] ) ->
                                                    mergeOrAppend (hexStringToString font text2) state

                                                ( SingleQuote, [ Text text2 ] ) ->
                                                    mergeOrAppend text2 state

                                                ( SingleQuote, [ HexString text2 ] ) ->
                                                    mergeOrAppend (hexStringToString font text2) state

                                                ( DoubleQuote, [ _, _, Text text2 ] ) ->
                                                    mergeOrAppend text2 state

                                                ( DoubleQuote, [ _, _, HexString text2 ] ) ->
                                                    mergeOrAppend (hexStringToString font text2) state

                                                ( TJ, variable ) ->
                                                    { text =
                                                        List.filterMap
                                                            (\a ->
                                                                case a of
                                                                    Text text2 ->
                                                                        Just text2

                                                                    HexString text2 ->
                                                                        hexStringToString font text2 |> Just

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                            variable
                                                            |> Array.fromList
                                                            |> Array.append state.text
                                                    , canMerge = True
                                                    }

                                                ( BDC, _ ) ->
                                                    state

                                                ( EMC, _ ) ->
                                                    state

                                                ( BMC, _ ) ->
                                                    state

                                                ( MP, _ ) ->
                                                    state

                                                ( DP, _ ) ->
                                                    state

                                                _ ->
                                                    { text = state.text, canMerge = False }
                                        )
                                        { text = Array.empty, canMerge = False }
                                        drawingInstructions
                                        |> .text
                                        |> Array.toList
                                        |> Ok
                                        |> Tuple.pair sections4

                                ( sections4, Ok _ ) ->
                                    ( sections4
                                    , Err
                                        [ { row = 0
                                          , col = 0
                                          , problem = Parser.Expecting "1"
                                          }
                                        ]
                                    )

                                ( sections4, Err error ) ->
                                    ( sections4, Err error )

                        ( _, ( sections3, _ ) ) ->
                            ( sections3
                            , Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Parser.Expecting "2"
                                  }
                                ]
                            )

                ( sections2, _ ) ->
                    ( sections2
                    , Err
                        [ { row = 0
                          , col = 0
                          , problem = Parser.Expecting "32"
                          }
                        ]
                    )

        _ ->
            ( sections
            , Err
                [ { row = 0
                  , col = 0
                  , problem = Parser.Expecting "3"
                  }
                ]
            )


parseSection2 :
    Config
    -> IndirectReference_
    -> Dict ( Int, Int ) Section
    -> ( Dict ( Int, Int ) Section, Result (List DeadEnd) Object )
parseSection2 config ref sections =
    case Dict.get ( ref.index, ref.revision ) sections of
        Just (IsNotParsed section) ->
            let
                result =
                    parseSection config section sections
            in
            ( Dict.insert ( ref.index, ref.revision ) (IsParsed result) sections, result )

        Just (IsParsed result) ->
            ( sections, result )

        Nothing ->
            ( sections
            , Err
                [ { row = ref.index
                  , col = ref.revision
                  , problem = Parser.Expecting "5"
                  }
                ]
            )


parseSection : Config -> { bytes : Bytes, text : String } -> Dict ( Int, Int ) Section -> Result (List DeadEnd) Object
parseSection config section sections =
    Parser.run
        (topLevelObjectParser config section.bytes section.text sections)
        section.text


handleEncryption : XRefTable -> Dict ( Int, Int ) Section -> ( Dict ( Int, Int ) Section, Config )
handleEncryption xRef sections0 =
    case ( Dict.get "Encrypt" xRef.metadata, Dict.get "ID" xRef.metadata ) of
        ( Just (IndirectReference ref), Just (PdfArray idData) ) ->
            case parseSection2 { encryption = Nothing } ref sections0 of
                ( sections1, Ok (PdfDict dict) ) ->
                    ( sections1
                    , case ( Dict.get "Length" dict, Dict.get "P" dict, Dict.get "O" dict ) of
                        ( Just (PdfInt length), Just (PdfInt pEntry), Just (Text ownerHash) ) ->
                            { encryption =
                                getRc4Key
                                    (length // 8)
                                    (encodeAscii ownerHash)
                                    (BE.unsignedInt32 LE pEntry |> BE.encode)
                                    (case Array.get 0 idData of
                                        Just (HexString hex) ->
                                            Hex.Convert.toBytes hex |> Maybe.withDefault emptyBytes

                                        _ ->
                                            emptyBytes
                                    )
                                    |> Just
                            }

                        _ ->
                            { encryption = Nothing }
                    )

                ( sections1, _ ) ->
                    ( sections1, { encryption = Nothing } )

        _ ->
            ( sections0, { encryption = Nothing } )


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
                ([ ( "Title", Text (title pdf_) ) ] |> Dict.fromList |> PdfDict)

        catalog : IndirectObject
        catalog =
            indirectObject
                catalogIndirectReference
                ([ ( "Type", Name "Catalog" ), ( "Pages", IndirectReference pageRootIndirectReference ) ]
                    |> Dict.fromList
                    |> PdfDict
                )

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
                , [ ( "Size", PdfInt xRefCount )
                  , ( "Info", IndirectReference infoIndirectReference )
                  , ( "Root", IndirectReference catalogIndirectReference )
                  ]
                    |> Dict.fromList
                    |> encodePdfDict
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
           , ( "Font"
             , List.indexedMap
                (\index _ ->
                    ( "F" ++ String.fromInt (index + 1)
                    , IndirectReference { index = index + fontIndexOffset, revision = 0 }
                    )
                )
                fonts
                |> Dict.fromList
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
                                |> Dict.fromList
                                |> PdfDict
                                |> Tuple.pair "XObject"
                                |> List.singleton

                        [] ->
                            []
                   )
                |> Dict.fromList
                |> PdfDict
           )
         ]
            |> Dict.fromList
            |> PdfDict
        )


fontObject : IndirectReference_ -> Font -> IndirectObject
fontObject indirectReference font_ =
    indirectObject
        indirectReference
        ([ ( "Type", Name "Font" )
         , ( "Subtype", Name "Type1" )
         , ( "BaseFont", Name (fontName font_) )
         , ( "Encoding", Name "WinAnsiEncoding" )
         ]
            |> Dict.fromList
            |> PdfDict
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
                (Dict.fromList
                    [ ( "Type", Name "XObject" )
                    , ( "Subtype", Name "Image" )
                    , ( "Width", PdfInt width )
                    , ( "Height", PdfInt height )
                    , ( "ColorSpace", Name "DeviceRGB" )
                    , ( "BitsPerComponent", PdfInt 8 )
                    , ( "Filter", Name "DCTDecode" )
                    ]
                )
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
                        ([ ( "Type", Name "Page" )
                         , mediaBox pageSize
                         , ( "Parent", IndirectReference pageRootIndirectReference )
                         , ( "Contents", IndirectReference contentIndirectReference )
                         ]
                            |> Dict.fromList
                            |> PdfDict
                        )
                , content =
                    indirectObject
                        contentIndirectReference
                        (Stream Dict.empty streamContent)
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
    { instructions : Array GraphicsInstruction
    , cursorPosition : Point2d Meters PageCoordinates
    , fontSize : Length
    , pageSize : Vector2d Meters PageCoordinates
    , fontIndex : Int
    }


initIntermediateInstructions : Vector2d Meters PageCoordinates -> IntermediateInstructions
initIntermediateInstructions pageSize =
    { instructions = Array.empty
    , cursorPosition = pageCoordToPdfCoord pageSize Point2d.origin
    , pageSize = pageSize
    , fontSize = Length.points -1
    , fontIndex = -1
    }


endIntermediateInstructions : IntermediateInstructions -> List GraphicsInstruction
endIntermediateInstructions intermediateInstructions =
    { operator = BT, parameters = [] }
        :: Array.toList intermediateInstructions.instructions
        ++ [ { operator = ET, parameters = [] } ]


drawImage : BoundingBox2d Meters PageCoordinates -> Int -> IntermediateInstructions -> IntermediateInstructions
drawImage bounds imageIndex intermediate =
    let
        ( width, height ) =
            BoundingBox2d.dimensions bounds

        { minX, maxY } =
            BoundingBox2d.extrema bounds

        ( x, y ) =
            Point2d.xy minX maxY |> pageCoordToPdfCoord intermediate.pageSize |> Point2d.toTuple Length.inPoints
    in
    { intermediate
        | instructions =
            Array.append
                intermediate.instructions
                (Array.fromList
                    [ { operator = QLowercase, parameters = [] }
                    , { operator = CmLowercase
                      , parameters =
                            [ Length.inPoints width |> PdfFloat
                            , PdfFloat 0
                            , PdfFloat 0
                            , Length.inPoints height |> PdfFloat
                            , PdfFloat x
                            , PdfFloat y
                            ]
                      }
                    , { operator = Do
                      , parameters =
                            [ Name "Im"
                            , PdfInt imageIndex
                            ]
                      }
                    , { operator = Q, parameters = [] }
                    ]
                )
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
    { intermediate | instructions = Array.push { operator = Tj, parameters = [ Text line ] } intermediate.instructions }


setFont : Int -> Length -> IntermediateInstructions -> IntermediateInstructions
setFont fontIndex fontSize intermediate =
    { intermediate
        | instructions =
            Array.push
                { operator = Tf, parameters = [ Name "F", PdfInt fontIndex, Length.inPoints fontSize |> PdfFloat ] }
                intermediate.instructions
    }


moveCursor : Vector2d Meters PageCoordinates -> IntermediateInstructions -> IntermediateInstructions
moveCursor offset intermediate =
    let
        ( x, y ) =
            Vector2d.toTuple Length.inPoints offset
    in
    { intermediate
        | instructions =
            Array.push
                { operator = Td, parameters = [ PdfFloat x, PdfFloat -y ] }
                intermediate.instructions
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
    | PdfDict (Dict String Object)
    | PdfArray (Array Object)
    | Text String
    | HexString String
    | Stream (Dict String Object) StreamContent
    | IndirectReference IndirectReference_
    | PdfBool Bool


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

        HexString text2 ->
            hexToString text2 |> BE.string

        Stream dict streamContent ->
            let
                ( streamContent_, dict2 ) =
                    case streamContent of
                        ResourceData data ->
                            ( data, Dict.insert "Length" (PdfInt (Bytes.width data)) dict )

                        DrawingInstructions text_ ->
                            let
                                deflate =
                                    False

                                textBytes : Bytes
                                textBytes =
                                    encodeGraphicsInstructions text_
                                        |> BE.encode
                                        |> (if deflate then
                                                Flate.deflateZlib

                                            else
                                                identity
                                           )
                            in
                            ( textBytes
                            , Dict.insert "Length" (PdfInt (Bytes.width textBytes)) dict
                                |> (if deflate then
                                        Dict.insert "Filter" (Name "FlateDecode")

                                    else
                                        identity
                                   )
                            )

                        ToUnicodeData _ ->
                            ( BE.string "" |> BE.encode, dict )
            in
            BE.sequence
                [ encodePdfDict dict2
                , BE.string "\nstream\n"
                , BE.bytes streamContent_
                , BE.string "\nendstream"
                ]

        IndirectReference { index, revision } ->
            String.fromInt index ++ " " ++ String.fromInt revision ++ " R" |> BE.string

        PdfBool bool ->
            if bool then
                BE.string "true"

            else
                BE.string "false"


encodeGraphicsInstructions : List GraphicsInstruction -> BE.Encoder
encodeGraphicsInstructions instructions =
    List.concatMap
        (\instruction ->
            List.intersperse (BE.string " ") (List.map encodeObject instruction.parameters)
                ++ [ BE.string " "
                   , operatorToString instruction.operator |> BE.string
                   , BE.string " "
                   ]
        )
        instructions
        |> BE.sequence


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


hexToString : String -> String
hexToString text2 =
    "<" ++ text2 ++ ">"


nameToString : String -> String
nameToString name =
    "/" ++ name


encodePdfDict : Dict String Object -> BE.Encoder
encodePdfDict dict =
    Dict.toList dict
        |> List.map (\( key, value ) -> BE.sequence [ BE.string (nameToString key ++ " "), encodeObject value ])
        |> List.intersperse (BE.string " ")
        |> (\a -> BE.string "<< " :: a ++ [ BE.string " >>" ])
        |> BE.sequence


type Operator
    = WLowercase
    | J
    | JLowercase
    | M
    | DLowercase
    | RiLowercase
    | ILowercase
    | GsLowercase
    | QLowercase
    | Q
    | CmLowercase
    | MLowercase
    | LLowercase
    | CLowercase
    | VLowercase
    | YLowercase
    | HLowercase
    | ReLowercase
    | S
    | SLowercase
    | FLowercase
    | F
    | FStarLowercase
    | B
    | BStar
    | BLowercase
    | BStarLowercase
    | NLowercase
    | W
    | WStar
    | BT
    | ET
    | Tc
    | Tw
    | Tz
    | TL
    | Tf
    | Tr
    | Ts
    | Td
    | TD
    | Tm
    | T
    | Tj
    | TJ
    | SingleQuote
    | DoubleQuote
    | D0Lowercase
    | D1Lowercase
    | CS
    | CsLowercase
    | SC
    | SCN
    | ScLowercase
    | ScnLowercase
    | G
    | GLowercase
    | RG
    | RgLowercase
    | K
    | KLowercase
    | ShLowercase
    | BI
    | ID
    | EI
    | Do
    | MP
    | DP
    | BMC
    | BDC
    | EMC
    | BX
    | EX


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        WLowercase ->
            "w"

        J ->
            "J"

        JLowercase ->
            "j"

        M ->
            "M"

        DLowercase ->
            "d"

        RiLowercase ->
            "ri"

        ILowercase ->
            "i"

        GsLowercase ->
            "gs"

        QLowercase ->
            "q"

        Q ->
            "Q"

        CmLowercase ->
            "cm"

        MLowercase ->
            "m"

        LLowercase ->
            "l"

        CLowercase ->
            "c"

        VLowercase ->
            "v"

        YLowercase ->
            "y"

        HLowercase ->
            "h"

        ReLowercase ->
            "re"

        S ->
            "S"

        SLowercase ->
            "s"

        FLowercase ->
            "f"

        F ->
            "F"

        FStarLowercase ->
            "f*"

        B ->
            "B"

        BStar ->
            "B*"

        BLowercase ->
            "b"

        BStarLowercase ->
            "b*"

        NLowercase ->
            "n"

        W ->
            "W"

        WStar ->
            "W*"

        BT ->
            "BT"

        ET ->
            "ET"

        Tc ->
            "Tc"

        Tw ->
            "Tw"

        Tz ->
            "Tz"

        TL ->
            "TL"

        Tf ->
            "Tf"

        Tr ->
            "Tr"

        Ts ->
            "Ts"

        Td ->
            "Td"

        TD ->
            "TD"

        Tm ->
            "Tm"

        T ->
            "T*"

        Tj ->
            "Tj"

        TJ ->
            "TJ"

        SingleQuote ->
            "'"

        DoubleQuote ->
            "\""

        D0Lowercase ->
            "d0"

        D1Lowercase ->
            "d1"

        CS ->
            "CS"

        CsLowercase ->
            "cs"

        SC ->
            "SC"

        SCN ->
            "SCN"

        ScLowercase ->
            "sc"

        ScnLowercase ->
            "scn"

        G ->
            "G"

        GLowercase ->
            "g"

        RG ->
            "RG"

        RgLowercase ->
            "rg"

        K ->
            "K"

        KLowercase ->
            "k"

        ShLowercase ->
            "sh"

        BI ->
            "BI"

        ID ->
            "ID"

        EI ->
            "EI"

        Do ->
            "Do"

        MP ->
            "MP"

        DP ->
            "DP"

        BMC ->
            "BMC"

        BDC ->
            "BDC"

        EMC ->
            "EMC"

        BX ->
            "BX"

        EX ->
            "EX"


operators : Dict String Operator
operators =
    [ ( "w", WLowercase )
    , ( "J", J )
    , ( "j", JLowercase )
    , ( "M", M )
    , ( "d", DLowercase )
    , ( "ri", RiLowercase )
    , ( "i", ILowercase )
    , ( "gs", GsLowercase )
    , ( "q", QLowercase )
    , ( "Q", Q )
    , ( "cm", CmLowercase )
    , ( "m", MLowercase )
    , ( "l", LLowercase )
    , ( "c", CLowercase )
    , ( "v", VLowercase )
    , ( "y", YLowercase )
    , ( "h", HLowercase )
    , ( "re", ReLowercase )
    , ( "S", S )
    , ( "s", SLowercase )
    , ( "f", FLowercase )
    , ( "F", F )
    , ( "f*", FStarLowercase )
    , ( "B", B )
    , ( "B*", BStar )
    , ( "b", BLowercase )
    , ( "b*", BStarLowercase )
    , ( "n", NLowercase )
    , ( "W", W )
    , ( "W*", WStar )
    , ( "BT", BT )
    , ( "ET", ET )
    , ( "Tc", Tc )
    , ( "Tw", Tw )
    , ( "Tz", Tz )
    , ( "TL", TL )
    , ( "Tf", Tf )
    , ( "Tr", Tr )
    , ( "Ts", Ts )
    , ( "Td", Td )
    , ( "TD", TD )
    , ( "Tm", Tm )
    , ( "T*", T )
    , ( "Tj", Tj )
    , ( "TJ", TJ )
    , ( "'", SingleQuote )
    , ( "\"", DoubleQuote )
    , ( "d0", D0Lowercase )
    , ( "d1", D1Lowercase )
    , ( "CS", CS )
    , ( "cs", CsLowercase )
    , ( "SC", SC )
    , ( "SCN", SCN )
    , ( "sc", ScLowercase )
    , ( "scn", ScnLowercase )
    , ( "G", G )
    , ( "g", GLowercase )
    , ( "RG", RG )
    , ( "rg", RgLowercase )
    , ( "K", K )
    , ( "k", KLowercase )
    , ( "sh", ShLowercase )
    , ( "BI", BI )
    , ( "ID", ID )
    , ( "EI", EI )
    , ( "Do", Do )
    , ( "MP", MP )
    , ( "DP", DP )
    , ( "BMC", BMC )
    , ( "BDC", BDC )
    , ( "EMC", EMC )
    , ( "BX", BX )
    , ( "EX", EX )
    ]
        |> Dict.fromList


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
    chompOneOrMore Char.isDigit
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


dictParser : Parser (Dict String Object)
dictParser =
    Parser.succeed identity
        |. Parser.symbol "<<"
        |. Parser.spaces
        |= Parser.loop
            []
            (\list ->
                Parser.oneOf
                    [ Parser.symbol ">>" |> Parser.map (\() -> Dict.fromList list |> Parser.Done)
                    , Parser.succeed (\key value -> ( key, value ) :: list |> Parser.Loop)
                        |= nameParser
                        |. Parser.spaces
                        |= objectParser
                        |. Parser.spaces
                    ]
            )
        |. Parser.spaces


type alias EncryptionData =
    { key : Bytes
    , reference : IndirectReference_
    }


streamParser :
    Config
    -> IndirectReference_
    -> Bytes
    -> String
    -> Dict String Object
    -> Dict ( Int, Int ) Section
    -> Parser (Maybe StreamContent)
streamParser config ref originalBytes originalText dict sections =
    let
        parserHelper : (Bytes -> Parser (Maybe a)) -> Parser (Maybe a)
        parserHelper a =
            case getValue "Length" config dict sections of
                ( sections2, Ok (PdfInt length) ) ->
                    Parser.succeed identity
                        |. Parser.symbol "stream"
                        |. Parser.oneOf [ Parser.symbol "\n", Parser.symbol "\u{000D}\n" ]
                        |= Parser.getPosition
                        |> Parser.andThen
                            (\( row, column ) ->
                                let
                                    offset : Int
                                    offset =
                                        String.split "\n" originalText
                                            |> List.take (row - 1)
                                            |> List.map (\text2 -> String.length text2 + 1)
                                            |> List.sum
                                            |> (+) (column - 1)
                                in
                                case sliceBytes offset length originalBytes of
                                    Just bytes ->
                                        case config.encryption of
                                            Just encryption ->
                                                decryptStream encryption ref bytes |> a

                                            Nothing ->
                                                a bytes

                                    Nothing ->
                                        Parser.problem "Failed to slice bytes"
                            )

                ( sections2, _ ) ->
                    Parser.succeed Nothing
                        |. Parser.symbol "endobj"
    in
    case Dict.get "Filter" dict of
        Just (Name "Standard") ->
            Parser.succeed Nothing

        Just (Name "FlateDecode") ->
            parserHelper
                (\bytes ->
                    case Flate.inflateZlib bytes of
                        Just inflated ->
                            case Parser.run streamContentParser (decodeAscii inflated) of
                                Ok ok ->
                                    Parser.succeed (Just ok)

                                Err error ->
                                    let
                                        _ =
                                            Debug.log "error" error
                                    in
                                    Parser.problem "Stream parsing failed 2"

                        Nothing ->
                            Parser.problem "Inflate failed"
                )

        Just _ ->
            Parser.problem "Unknown filter type"

        Nothing ->
            parserHelper
                (\bytes ->
                    case Parser.run streamContentParser (decodeAscii bytes) of
                        Ok ok ->
                            Parser.succeed (Just ok)

                        Err _ ->
                            Parser.problem "Stream parsing failed"
                )


encodeAscii : String -> Bytes
encodeAscii text2 =
    String.toList text2
        |> List.map (\char -> Char.toCode char |> Bitwise.and 0xFF |> BE.unsignedInt8)
        |> BE.sequence
        |> BE.encode


decodeAscii : Bytes -> String
decodeAscii bytes =
    BD.decode
        (BD.loop
            ( Bytes.width bytes, [] )
            (\( remaining, list ) ->
                if remaining <= 0 then
                    List.reverse list |> String.fromList |> BD.Done |> BD.succeed

                else
                    BD.unsignedInt8 |> BD.map (\byte -> BD.Loop ( remaining - 1, Char.fromCode byte :: list ))
            )
        )
        bytes
        |> Maybe.withDefault ""


type alias GraphicsInstruction =
    { operator : Operator, parameters : List Object }


streamContentParser : Parser StreamContent
streamContentParser =
    Parser.oneOf
        [ Parser.succeed (\first second -> Dict.union first second |> ToUnicodeData)
            |. Parser.symbol "/CIDInit"
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.chompUntil "beginbfchar"
                    |. Parser.symbol "beginbfchar"
                    |. Parser.spaces
                    |= Parser.loop
                        []
                        (\list ->
                            Parser.oneOf
                                [ Parser.symbol "endbfchar"
                                    |> Parser.map
                                        (\() -> Dict.fromList list |> Parser.Done)
                                , Parser.succeed Tuple.pair
                                    |= hexStringParser
                                    |. Parser.spaces
                                    |= hexStringParser
                                    |. Parser.spaces
                                    |> Parser.andThen
                                        (\( first, second ) ->
                                            case hex4ToInt second of
                                                Just value ->
                                                    ( first, Char.fromCode value ) :: list |> Parser.Loop |> Parser.succeed

                                                Nothing ->
                                                    Parser.problem "Invalid unicode value"
                                        )
                                ]
                        )
                , Parser.succeed Dict.empty
                ]
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.chompUntil "beginbfrange"
                    |. Parser.symbol "beginbfrange"
                    |. Parser.spaces
                    |= Parser.loop
                        []
                        (\list ->
                            Parser.oneOf
                                [ Parser.symbol "endbfrange"
                                    |> Parser.map
                                        (\() -> Dict.fromList list |> Parser.Done)
                                , Parser.succeed (\a b c -> ( a, b, c ))
                                    |= hexStringParser
                                    |. Parser.spaces
                                    |= hexStringParser
                                    |. Parser.spaces
                                    |= hexStringParser
                                    |. Parser.spaces
                                    |> Parser.andThen
                                        (\( rangeStart, rangeEnd, targetRangeStart ) ->
                                            case ( hex4ToInt rangeStart, hex4ToInt rangeEnd, hex4ToInt targetRangeStart ) of
                                                ( Just start, Just end, Just targetStart ) ->
                                                    List.map
                                                        (\index ->
                                                            ( BE.unsignedInt16 BE index
                                                                |> BE.encode
                                                                |> Hex.Convert.toString
                                                            , targetStart + index - start |> Char.fromCode
                                                            )
                                                        )
                                                        (List.range start end)
                                                        ++ list
                                                        |> Parser.Loop
                                                        |> Parser.succeed

                                                _ ->
                                                    Parser.problem "Invalid unicode value"
                                        )
                                ]
                        )
                , Parser.succeed Dict.empty
                ]
        , Parser.loop
            []
            (\list ->
                Parser.oneOf
                    [ Parser.succeed (\item -> item :: list |> Parser.Loop)
                        |= graphicsParser
                        |. Parser.spaces
                    , Parser.end |> Parser.map (\() -> List.reverse list |> DrawingInstructions |> Parser.Done)
                    ]
            )
        ]


hex4ToInt text2 =
    Hex.Convert.toBytes text2 |> Maybe.andThen (BD.decode (BD.unsignedInt16 BE))


graphicsParser : Parser GraphicsInstruction
graphicsParser =
    Parser.loop
        Array.empty
        (\array ->
            Parser.oneOf
                [ Parser.succeed (\items -> Array.append array (Array.fromList items) |> Parser.Loop)
                    |= objectInArrayParser
                    |. Parser.spaces
                , chompOneOrMore (\char -> Char.isAlpha char || char == '*' || char == '"' || char == '\'')
                    |> Parser.andThen
                        (\text2 ->
                            case Dict.get text2 operators of
                                Just operator ->
                                    { operator = operator, parameters = Array.toList array } |> Parser.Done |> Parser.succeed

                                Nothing ->
                                    Parser.problem ("Invalid operator " ++ text2)
                        )
                ]
        )


topLevelReferenceParser : Parser IndirectReference_
topLevelReferenceParser =
    Parser.succeed (\index revision -> { index = index, revision = revision })
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol "obj"


topLevelObjectParser : Config -> Bytes -> String -> Dict ( Int, Int ) Section -> Parser Object
topLevelObjectParser config originalBytes originalText sections =
    Parser.succeed Tuple.pair
        |= topLevelReferenceParser
        |. Parser.spaces
        |= objectParser
        |> Parser.andThen
            (\( reference, object ) ->
                case object of
                    PdfDict dict ->
                        streamParser config reference originalBytes originalText dict sections
                            |> Parser.map
                                (\maybeStream ->
                                    case maybeStream of
                                        Just stream ->
                                            Stream dict stream

                                        Nothing ->
                                            PdfDict dict
                                )

                    _ ->
                        Parser.succeed object
            )


sliceBytes : Int -> Int -> Bytes -> Maybe Bytes
sliceBytes offset size bytes =
    BD.decode (BD.map2 (\_ a -> a) (BD.bytes offset) (BD.bytes size)) bytes


basicObjectParser : Parser Object
basicObjectParser =
    Parser.oneOf
        [ dictParser |> Parser.map PdfDict
        , nameParser |> Parser.map Name
        , textParser
        , hexStringParser |> Parser.map HexString
        , arrayParser
        , Parser.symbol "true" |> Parser.map (\_ -> PdfBool True)
        , Parser.symbol "false" |> Parser.map (\_ -> PdfBool False)
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
    Parser.andThen
        (\first ->
            case first of
                IsFloat float ->
                    Parser.succeed [ PdfFloat float ]

                IsInt firstInt ->
                    Parser.succeed identity
                        |. Parser.spaces
                        |= Parser.oneOf
                            [ Parser.andThen
                                (\second ->
                                    case second of
                                        IsFloat float ->
                                            Parser.succeed [ PdfInt firstInt, PdfFloat float ]

                                        IsInt secondInt ->
                                            Parser.succeed identity
                                                |. Parser.spaces
                                                |= Parser.oneOf
                                                    [ Parser.succeed [ IndirectReference { index = firstInt, revision = secondInt } ]
                                                        |. Parser.symbol "R"
                                                    , Parser.succeed (\third -> [ PdfInt firstInt, PdfInt secondInt, PdfInt third ])
                                                        |= backtrackableIntParser
                                                    , Parser.succeed (\third -> [ PdfInt firstInt, PdfInt secondInt, PdfFloat third ])
                                                        |= Parser.float
                                                    , Parser.succeed [ PdfInt firstInt, PdfInt secondInt ]
                                                    ]
                                )
                                floatOrIntParser
                            , Parser.succeed [ PdfInt firstInt ]
                            ]
        )
        floatOrIntParser


type FloatOrInt
    = IsFloat Float
    | IsInt Int


floatOrIntParser : Parser FloatOrInt
floatOrIntParser =
    Parser.succeed
        (\negate ( maybeNumber, maybeDecimal ) ->
            let
                negateText : String
                negateText =
                    if negate then
                        "-"

                    else
                        ""

                number : String
                number =
                    Maybe.withDefault "0" maybeNumber
            in
            case maybeDecimal of
                Just decimal ->
                    case negateText ++ number ++ "." ++ decimal |> String.toFloat of
                        Just float ->
                            IsFloat float

                        Nothing ->
                            -- Should never happen
                            IsFloat 123

                Nothing ->
                    case negateText ++ number |> String.toInt of
                        Just int ->
                            IsInt int

                        Nothing ->
                            -- Should never happen
                            IsInt 321
        )
        |= Parser.oneOf
            [ Parser.symbol "-" |> Parser.map (\() -> True)
            , Parser.succeed False
            ]
        |= Parser.oneOf
            [ Parser.succeed (\decimal -> ( Nothing, Just decimal ))
                |. Parser.symbol "."
                |= chompOneOrMore Char.isDigit
            , Parser.succeed (\number maybeDecimal -> ( Just number, maybeDecimal ))
                |= chompOneOrMore Char.isDigit
                |= Parser.oneOf
                    [ Parser.succeed Just
                        |. Parser.symbol "."
                        |= chompOneOrMore Char.isDigit
                    , Parser.succeed Nothing
                    ]
            ]


chompOneOrMore : (Char -> Bool) -> Parser String
chompOneOrMore func =
    Parser.succeed (\first rest -> first ++ rest)
        |= Parser.getChompedString (Parser.chompIf func)
        |= Parser.getChompedString (Parser.chompWhile func)


backtrackableIntParser : Parser Int
backtrackableIntParser =
    Parser.backtrackable Parser.int


objectInArrayParser : Parser (List Object)
objectInArrayParser =
    Parser.oneOf
        [ intFloatOrIndirectReferenceParser
        , basicObjectParser |> Parser.map List.singleton
        ]


paddingBytes : Bytes
paddingBytes =
    "28BF4E5E4E758A4164004E56FFFA01082E2E00B6D0683E802F0CA9FE6453697A"
        |> Hex.Convert.toBytes
        |> Maybe.withDefault emptyBytes


emptyBytes : Bytes
emptyBytes =
    BE.sequence [] |> BE.encode


getRc4Key : Int -> Bytes -> Bytes -> Bytes -> Bytes
getRc4Key hashByteLength oEntry pEntry idEntry =
    let
        initialHash : Bytes
        initialHash =
            BE.sequence
                [ BE.bytes paddingBytes
                , BE.bytes oEntry
                , BE.bytes pEntry
                , BE.bytes idEntry
                ]
                |> BE.encode
    in
    List.foldl
        (\_ hash -> Md5.fromBytes hash |> Hex.Convert.toBytes |> Maybe.withDefault emptyBytes)
        initialHash
        -- Hash 51 times
        (List.range 1 51)
        |> sliceBytes 0 hashByteLength
        |> Maybe.withDefault emptyBytes


bytesToInts : Bytes -> Maybe (List Int)
bytesToInts bytes =
    BD.decode
        (BD.loop
            ( Bytes.width bytes, [] )
            (\( remaining, list ) ->
                if remaining <= 0 then
                    List.reverse list |> BD.Done |> BD.succeed

                else
                    BD.unsignedInt8
                        |> BD.map (\a -> ( remaining - 1, a :: list ) |> BD.Loop)
            )
        )
        bytes


decryptStream : Bytes -> IndirectReference_ -> Bytes -> Bytes
decryptStream encryptionKey ref stream =
    let
        key : Bytes
        key =
            BE.sequence
                [ BE.bytes encryptionKey
                , BE.unsignedInt8 (Bitwise.and 0xFF ref.index)
                , BE.unsignedInt8 (Bitwise.and 0xFF (Bitwise.shiftRightBy 8 ref.index))
                , BE.unsignedInt8 (Bitwise.and 0xFF (Bitwise.shiftRightBy 16 ref.index))
                , BE.unsignedInt8 (Bitwise.and 0xFF ref.revision)
                , BE.unsignedInt8 (Bitwise.and 0xFF (Bitwise.shiftRightBy 8 ref.revision))
                ]
                |> BE.encode

        key2 =
            key
                |> Md5.fromBytes
                |> Hex.Convert.toBytes
                |> Maybe.withDefault emptyBytes
    in
    Rc4_2.decrypt key2 stream


textParser : Parser Object
textParser =
    Parser.succeed Text
        |. Parser.symbol "("
        |= textParserHelper ')'


hexStringParser : Parser String
hexStringParser =
    Parser.succeed identity
        |. Parser.symbol "<"
        |= textParserHelper '>'


textParserHelper : Char -> Parser String
textParserHelper delimiter =
    Parser.loop
        ""
        (\state ->
            Parser.oneOf
                [ Parser.symbol (String.fromChar delimiter) |> Parser.map (\() -> Parser.Done state)
                , Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.symbol "\\"
                        |= Parser.oneOf
                            [ Parser.symbol "\\" |> Parser.map (\() -> "\\")
                            , Parser.symbol ")" |> Parser.map (\() -> ")")
                            , Parser.symbol "(" |> Parser.map (\() -> "(")
                            , Parser.symbol "n" |> Parser.map (\() -> "\n")
                            , Parser.symbol "r" |> Parser.map (\() -> "\u{000D}")
                            , Parser.symbol "t" |> Parser.map (\() -> "\t")
                            , Parser.symbol "b" |> Parser.map (\() -> "")
                            , Parser.symbol "f" |> Parser.map (\() -> "")
                            , Parser.succeed
                                (\one two three -> Char.fromCode (8 * 8 * one + 8 * two + three) |> String.fromChar)
                                |= chompDigit
                                |= chompDigit
                                |= chompDigit
                            ]
                    , chompOneOrMore (\char -> char /= '\\' && char /= delimiter)
                    ]
                    |> Parser.map (\text2 -> state ++ text2 |> Parser.Loop)
                ]
        )


chompDigit : Parser Int
chompDigit =
    Parser.getChompedString (Parser.chompIf Char.isDigit)
        |> Parser.map (\char -> String.toInt char |> Maybe.withDefault 0)


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
                        |. Parser.spaces
                    , Parser.symbol "]" |> Parser.map (\() -> Parser.Done array)
                    ]
            )


nameParser : Parser String
nameParser =
    Parser.succeed identity
        |. Parser.symbol "/"
        |= chompOneOrMore
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
    | DrawingInstructions (List GraphicsInstruction)
    | ToUnicodeData (Dict String Char)


floatToString : Float -> String
floatToString =
    Round.round 5



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
