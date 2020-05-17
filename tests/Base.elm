module Base exposing (tests)

import Expect exposing (Expectation)
import Length
import Pdf
import Point2d
import Test exposing (Test, describe, test)
import Vector2d


tests =
    describe "tests"
        [ test "generate hallo wald" <|
            \_ ->
                let
                    expected =
                        String.replace "\u{000D}\n"
                            "\n"
                            """%PDF-1.4
1 0 obj  % entry point
<< /Title (test) >>
endobj
2 0 obj
<< /Type /Catalog /Pages 3 0 R >>
endobj
3 0 obj
<< /Kids [ 5 0 R 6 0 R ] /Count 1 /Type /Pages /Resources << /Font << /F1 4 0 R >> /PRocSet [ /PDF /Text ] >> >>
endobj
4 0 obj
<< /Type /Page /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>
endobj
5 0 obj
<< /Type /Page /MediaBox [ 0 0 1100.00000 100.00000 ] /Parent 3 0 R /Contents 6 0 R >>
endobj
6 0 obj
<< /Length 114 >>
stream
BT /F1 36.00000 Tf 10.00000 30.00000 Td (If you can read this then it's possible to encode PDFs with Elm!) Tj ET
endstream
endobj
xref
0 7
0000000000 65535 f
0000000009 00049 n
0000000059 00048 n
0000000108 00127 n
0000000236 00096 n
0000000333 00101 n
0000000435 00162 n
trailer
<< /Size 7 /Info 1 0 R /Root 2 0 R >>
startxref
598
%%EOF"""
                in
                Pdf.encode
                    (Pdf.init
                        { title = "test"
                        , firstPage =
                            Pdf.page
                                (Vector2d.fromTuple Length.points ( 1100, 100 ))
                                [ Pdf.textBox
                                    (Length.points 36)
                                    Nothing
                                    (Point2d.fromTuple Length.points ( 10, 30 ))
                                    "If you can read this then it's possible to encode PDFs with Elm!"
                                ]
                        }
                    )
                    |> Debug.log ""
                    |> Expect.equal expected
        ]
