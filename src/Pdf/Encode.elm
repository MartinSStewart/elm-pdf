module Pdf.Encode exposing (Pdf, encode, init)

import Bytes exposing (Bytes)
import Bytes.Encode


encode : Pdf -> String
encode pdf =
    ""


encodeObject : Int -> String -> String
encodeObject index contents =
    String.fromInt index ++ " 0 obj\n<< " ++ contents ++ " >>\n" ++ "endobj"


escapeText : String -> String
escapeText =
    String.replace "\\" "\\\\" >> String.replace ")" "\\)" >> String.replace "(" "\\("


encodeTitleObject : Int -> String -> String
encodeTitleObject index title =
    encodeObject index ("/Title (" ++ escapeText title ++ ")")
