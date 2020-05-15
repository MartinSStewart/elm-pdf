module Pdf.Encode exposing (pdfToString)

import Bytes exposing (Bytes)
import Bytes.Encode
import Length exposing (Length)
import Pdf exposing (Pdf)
import Round


pdfToString : Pdf -> String
pdfToString pdf =
    "%PDF-1.7\n\n"
        ++ pdf
        ++ """xref
0 6
0000000000 65535 f
0000000010 00000 n
0000000079 00000 n
0000000173 00000 n
0000000301 00000 n
0000000380 00000 n
trailer
<<
/Size 6
/Root 1 0 R
>>
startxref
492
%%EOF"""


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
