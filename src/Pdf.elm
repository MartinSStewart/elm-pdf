module Pdf exposing
    ( pdf, page, textBox, Pdf, Page, Item, PageCoordinates
    , encoder
    )

{-| In order to use this package you'll need to install [`ianmackenzie/elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/) and [`ianmackenzie/elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/).

@docs pdf, page, textBox, encode, Pdf, Page, Item, PageCoordinates

-}

import Bytes exposing (Bytes)
import Bytes.Encode as BE
import Flate
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Round
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
    = TextBox
        { position : Point2d Meters PageCoordinates
        , text : String
        , fontSize : Length
        }


textBox : Length -> Point2d Meters PageCoordinates -> String -> Item
textBox fontSize position text =
    TextBox
        { position = position
        , text = text
        , fontSize = fontSize
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

        fontIndirectReference =
            { index = 4, revision = 0 }

        info : IndirectObject
        info =
            entryPoint
                infoIndirectReference
                (PdfDict [ ( "Title", Text (title pdf_) ) ])

        catalog : IndirectObject
        catalog =
            indirectObject
                catalogIndirectReference
                (PdfDict [ ( "Type", Name "Catalog" ), ( "Pages", IndirectReference pageRootIndirectReference ) ])

        pageRoot : IndirectObject
        pageRoot =
            indirectObject
                pageRootIndirectReference
                ([ ( "Kids"
                   , allPages
                        |> List.map (.page >> indirectObjectToIndirectReference >> IndirectReference)
                        |> PdfArray
                   )
                 , ( "Count", PdfInt (List.length (pages pdf_)) )
                 , ( "Type", Name "Pages" )
                 , ( "Resources"
                   , PdfDict
                        [ ( "Font", PdfDict [ ( "F1", IndirectReference fontIndirectReference ) ] )
                        , ( "PRocSet", PdfArray [ Name "PDF", Name "Text" ] )
                        ]
                   )
                 ]
                    |> PdfDict
                )

        font : IndirectObject
        font =
            indirectObject
                fontIndirectReference
                (PdfDict
                    [ ( "Type", Name "Page" )
                    , ( "Subtype", Name "Type1" )
                    , ( "BaseFont", Name "Helvetica" )
                    , ( "Encoding", Name "WinAnsiEncoding" )
                    ]
                )

        allPages : List { page : IndirectObject, content : IndirectObject }
        allPages =
            pages pdf_
                |> List.indexedMap
                    (\index (Page pageSize pageText) ->
                        let
                            contentIndirectReference =
                                { index = index * 2 + 6, revision = 0 }

                            streamContent : String
                            streamContent =
                                List.foldl
                                    (\(TextBox { position, text, fontSize }) previous ->
                                        let
                                            ( x, y ) =
                                                Point2d.toTuple Length.inPoints position

                                            writeLine : String -> String
                                            writeLine text_ =
                                                " " ++ textToString text_ ++ " Tj"

                                            lineSpacing : String
                                            lineSpacing =
                                                lengthToString (Quantity.negate fontSize)
                                        in
                                        case String.lines text of
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
                                                in
                                                previous
                                                    ++ (" /F1 " ++ lengthToString fontSize ++ " Tf ")
                                                    ++ (floatToString x ++ " " ++ floatToString y ++ " Td")
                                                    ++ writeLine head
                                                    ++ restOfLines

                                            [] ->
                                                ""
                                    )
                                    ""
                                    pageText
                                    |> (\a -> "BT" ++ a ++ " ET")
                        in
                        { page =
                            indirectObject
                                { index = index * 2 + 5, revision = 0 }
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
                                (Stream [] (DrawingInstructions streamContent))
                        }
                    )

        ( content, xRef ) =
            info
                :: catalog
                :: pageRoot
                :: font
                :: List.concatMap (\a -> [ a.page, a.content ]) allPages
                |> contentToBytes

        xRefToString : XRef -> BE.Encoder
        xRefToString (XRef xRefs) =
            let
                xRefLine { offset, size } =
                    String.padLeft 10 '0' (String.fromInt offset)
                        ++ " "
                        ++ String.padLeft 5 '0' (String.fromInt size)
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


contentToBytes : List IndirectObject -> ( Bytes, List { offset : Int, size : Int } )
contentToBytes =
    List.sortBy indirectObjectIndex
        >> List.foldl
            (\indirectObject_ ( content_, xRef_, index ) ->
                let
                    bytes : Bytes
                    bytes =
                        indirectObjectToString indirectObject_ |> BE.encode
                in
                ( BE.sequence [ BE.bytes content_, BE.bytes bytes, BE.string "\n" ] |> BE.encode
                , { offset = Bytes.width content_, size = Bytes.width bytes } :: xRef_
                , index + 1
                )
            )
            ( "%PDF-" ++ pdfVersion ++ "\n%éééé\n" |> BE.string |> BE.encode, [], 1 )
        >> (\( content, xRef, _ ) -> ( content, List.reverse xRef ))


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
    = XRef (List { offset : Int, size : Int })


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

                        DrawingInstructions text ->
                            let
                                textBytes =
                                    text |> BE.string |> BE.encode |> Flate.deflateZlib
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
