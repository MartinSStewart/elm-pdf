module Pdf.Encode exposing (pdfToString)

import Bytes exposing (Bytes)
import Bytes.Encode
import Length exposing (Length)
import Pdf exposing (Pdf)
import Round


pdfToString : Pdf -> String
pdfToString pdf =
    let
        info =
            PdfDict
                [ ( Name "Title", Text_ (Text (Pdf.title pdf)) )
                , ( Name "Pages", IndirectReference_ (IndirectReference { index = 2, revision = 0 }) )
                ]
                |> PdfDict_

        catalog =
            PdfDict
                [ ( Name "Type", Name_ (Name "Catalog") )
                , ( Name "Pages", IndirectReference_ (IndirectReference { index = 2, revision = 0 }) )
                ]
                |> PdfDict_

        pages =
            PdfDict
                []

        content =
            "%PDF-1.7\n\n"
                ++ indirectObjectToString 1 0 info
                ++ indirectObjectToString 2 0 catalog
    in
    content
        ++ xRefToString (XRef [])
        ++ String.fromInt (String.length content)
        ++ "\n%%EOF"


type XRef
    = XRef (List { offset : Int, size : Int, isFree : Bool })


xRefToString : XRef -> String
xRefToString (XRef xRefs) =
    let
        xRefLine { offset, size, isFree } =
            String.padLeft 10 '0' (String.fromInt offset)
                ++ " "
                ++ String.padLeft 5 '0' (String.fromInt size)
                ++ " "
                ++ (if isFree then
                        "f"

                    else
                        "n"
                   )
                ++ "\n"
    in
    "xref\n"
        ++ ("0 " ++ String.fromInt (List.length xRefs) ++ "\n")
        ++ (List.map xRefLine xRefs |> String.concat)
        ++ "trailer\n"
        ++ dictionaryToString
            (PdfDict
                [ ( Name "Size", Int_ (List.length xRefs) )
                , ( Name "Info", IndirectReference_ (IndirectReference { index = 1, revision = 0 }) )
                , ( Name "Root", IndirectReference_ (IndirectReference { index = 2, revision = 0 }) )
                ]
            )
        ++ "startxref\n"


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
                |> String.concat
    in
    "<<" ++ contentText ++ ">>"


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



--mediaBoxTag : { width : Length, height : Length } -> Name
--mediaBoxTag { width, height } =
--    Name { name = "MediaBox", contents = PdfArray [ Float_ 0, Float_ 0, Float_ (Length.inPoints width), Float_ (Length.inPoints height) ] |> PdfArray_ }
