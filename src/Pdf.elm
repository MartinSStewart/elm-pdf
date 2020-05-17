module Pdf exposing (Page, Pdf, TextBox, encode, page, pages, pdf, textBox, title)

import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Round
import Vector2d exposing (Vector2d)


type PageCoordinates
    = PageCoordinates Never


type Pdf
    = Pdf { title : String, pages : List Page }


type Page
    = Page (Vector2d Meters PageCoordinates) (List TextBox)


type TextBox
    = TextBox
        { position : Point2d Meters PageCoordinates
        , maxWidth : Maybe Length
        , text : String
        , fontSize : Length
        }


textBox : Length -> Maybe Length -> Point2d Meters PageCoordinates -> String -> TextBox
textBox fontSize maxWidth position text =
    TextBox
        { position = position
        , maxWidth = maxWidth
        , text = text
        , fontSize = fontSize
        }


page : Vector2d Meters PageCoordinates -> List TextBox -> Page
page =
    Page


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


encode : Pdf -> String
encode pdf_ =
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

                            streamContent =
                                List.foldl
                                    (\(TextBox { position, maxWidth, text, fontSize }) previous ->
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
                                (Stream [] (StreamContent streamContent))
                        }
                    )

        ( content, xRef, _ ) =
            (info :: catalog :: pageRoot :: font :: List.concatMap (\a -> [ a.page, a.content ]) allPages)
                |> List.sortBy indirectObjectIndex
                |> List.foldl
                    (\indirectObject_ ( content_, xRef_, index ) ->
                        let
                            text_ =
                                indirectObjectToString indirectObject_
                        in
                        ( content_ ++ text_ ++ "\n"
                        , { offset = String.length content_, size = String.length text_ } :: xRef_
                        , index + 1
                        )
                    )
                    ( "%PDF-" ++ pdfVersion ++ "\n", [], 1 )

        xRefToString : XRef -> String
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
            "xref\n"
                ++ ("0 " ++ String.fromInt xRefCount ++ "\n")
                ++ "0000000000 65535 f\n"
                ++ (List.map xRefLine xRefs |> String.concat)
                ++ "trailer\n"
                ++ pdfDictToString
                    [ ( "Size", PdfInt xRefCount )
                    , ( "Info", IndirectReference infoIndirectReference )
                    , ( "Root", IndirectReference catalogIndirectReference )
                    ]
                ++ "\nstartxref\n"
    in
    content
        ++ xRefToString (XRef (List.reverse xRef))
        ++ String.fromInt (String.length content)
        ++ "\n%%EOF"


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


objectToString : Object -> String
objectToString object =
    case object of
        Name name ->
            nameToString name

        PdfFloat float ->
            floatToString float

        PdfInt int ->
            String.fromInt int

        PdfDict pdfDict ->
            pdfDictToString pdfDict

        PdfArray pdfArray ->
            let
                contentText =
                    List.map objectToString pdfArray |> List.intersperse " " |> String.concat
            in
            "[ " ++ contentText ++ " ]"

        Text text_ ->
            textToString text_

        Stream dict (StreamContent content) ->
            let
                dict2 =
                    ( "Length", PdfInt (String.length content + 2) ) :: dict
            in
            pdfDictToString dict2 ++ "\nstream\n" ++ content ++ "\nendstream"

        IndirectReference { index, revision } ->
            String.fromInt index ++ " " ++ String.fromInt revision ++ " R"


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


pdfDictToString : List ( String, Object ) -> String
pdfDictToString =
    List.map (\( key, value ) -> nameToString key ++ " " ++ objectToString value)
        >> List.intersperse " "
        >> String.concat
        >> (\a -> "<< " ++ a ++ " >>")


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


indirectObjectToString : IndirectObject -> String
indirectObjectToString (IndirectObject { index, revision, isEntryPoint, object }) =
    String.fromInt index
        ++ " "
        ++ String.fromInt revision
        ++ " obj"
        ++ (if isEntryPoint then
                "  % entry point\n"

            else
                "\n"
           )
        ++ objectToString object
        ++ "\nendobj"


indirectObjectIndex : IndirectObject -> Int
indirectObjectIndex (IndirectObject { index }) =
    index


type StreamContent
    = StreamContent String


floatToString : Float -> String
floatToString =
    Round.round 5


lengthToString : Length -> String
lengthToString =
    Length.inPoints >> floatToString
