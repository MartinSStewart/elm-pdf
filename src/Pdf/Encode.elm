module Pdf.Encode exposing (encode)

import Bytes exposing (Bytes)
import Bytes.Encode
import Length exposing (Length)
import Pdf exposing (Pdf)
import Round


encode : Pdf -> String
encode pdf =
    ""


type Tag
    = Tag { name : String, contents : List TagContents }

type TagContents =
    TagArray PdfArray
    |

tagToString : Tag -> String
tagToString (Tag { name, contents }) =
    case contents of
        Just contents_ ->
            "/" ++ name ++ " " ++ contents_

        Nothing ->
            "/" ++ name


type PdfDict
    = PdfDict (List Tag) (List PdfDict)


dictionary : List Tag -> List PdfDict -> PdfDict
dictionary tags subdictionaries =
    PdfDict tags subdictionaries


dictionaryToString : PdfDict -> String
dictionaryToString (PdfDict tags dictionaries) =
    let
        tagsText =
            List.map tagToString tags |> List.intersperse " " |> String.concat

        subdictionariesText =
            List.map dictionaryToString dictionaries |> List.intersperse " " |> String.concat
    in
    "<<" ++ tagsText ++ " " ++ subdictionariesText ++ ">>"


type Object
    = Object PdfDict (Maybe Stream)


object : PdfDict -> Maybe Stream -> Object
object dict stream =
    Object dict stream


objectToString : Int -> Object -> String
objectToString index (Object dict stream) =
    let
        streamText =
            case stream of
                Just stream_ ->
                    "\n" ++ streamToString stream_

                Nothing ->
                    ""
    in
    String.fromInt index ++ " 0 obj\n" ++ dictionaryToString dict ++ streamText ++ "\nendobject"


type Stream
    = Stream String


streamToString : Stream -> String
streamToString (Stream a) =
    "stream " ++ a ++ "endstream"


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
    = PdfArray (List PdfFloat)


type PdfFloat
    = PdfFloat Float


float float_ =
    PdfFloat float_


floatToString : PdfFloat -> String
floatToString (PdfFloat float_) =
    Round.round 5 float_


array : List Float -> PdfArray
array floats =
    List.map float floats |> PdfArray


mediaBoxTag : { width : Length, height : Length } -> Tag
mediaBoxTag { width, height } =
    Tag { name = "MediaBox", contents = PdfArray [ 0, 0, PdfFloat (Length.inPoints width), PdfFloat (Length.inPoints height) ] }
