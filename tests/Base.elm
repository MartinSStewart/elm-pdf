module Base exposing (tests)

import Expect exposing (Expectation)
import Length
import Pdf
import Point2d
import Test exposing (Test, describe, test)
import Vector2d


tests =
    describe "tests"
        [ test "Single page PDF" <|
            \_ ->
                let
                    expected =
                        String.replace "\u{000D}\n"
                            "\n"
                            """%PDF-1.7
1 0 obj  % entry point
<< /Title (test) >>
endobj
2 0 obj
<< /Type /Catalog /Pages 3 0 R >>
endobj
3 0 obj
<< /Kids [ 5 0 R ] /Count 1 /Type /Pages /Resources << /Font << /F1 4 0 R >> /PRocSet [ /PDF /Text ] >> >>
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
0000000108 00121 n
0000000230 00096 n
0000000327 00101 n
0000000429 00162 n
trailer
<< /Size 7 /Info 1 0 R /Root 2 0 R >>
startxref
592
%%EOF"""
                in
                Pdf.encode
                    (Pdf.pdf
                        "test"
                        [ Pdf.page
                            (Vector2d.fromTuple Length.points ( 1100, 100 ))
                            [ Pdf.textBox
                                (Length.points 36)
                                Nothing
                                (Point2d.fromTuple Length.points ( 10, 30 ))
                                "If you can read this then it's possible to encode PDFs with Elm!"
                            ]
                        ]
                    )
                    |> Expect.equal expected
        , test "Two page PDF" <|
            \_ ->
                let
                    expected =
                        String.replace "\u{000D}\n"
                            "\n"
                            """%PDF-1.7
1 0 obj  % entry point
<< /Title (Two pages) >>
endobj
2 0 obj
<< /Type /Catalog /Pages 3 0 R >>
endobj
3 0 obj
<< /Kids [ 5 0 R 7 0 R ] /Count 2 /Type /Pages /Resources << /Font << /F1 4 0 R >> /PRocSet [ /PDF /Text ] >> >>
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
7 0 obj
<< /Type /Page /MediaBox [ 0 0 400.00000 100.00000 ] /Parent 3 0 R /Contents 8 0 R >>
endobj
8 0 obj
<< /Length 65 >>
stream
BT /F1 36.00000 Tf 40.00000 30.00000 Td (The second page) Tj ET
endstream
endobj
xref
0 9
0000000000 65535 f
0000000009 00054 n
0000000064 00048 n
0000000113 00127 n
0000000241 00096 n
0000000338 00101 n
0000000440 00162 n
0000000603 00100 n
0000000704 00112 n
trailer
<< /Size 9 /Info 1 0 R /Root 2 0 R >>
startxref
817
%%EOF"""
                in
                Pdf.encode
                    (Pdf.pdf
                        "Two pages"
                        [ Pdf.page
                            (Vector2d.fromTuple Length.points ( 1100, 100 ))
                            [ Pdf.textBox
                                (Length.points 36)
                                Nothing
                                (Point2d.fromTuple Length.points ( 10, 30 ))
                                "If you can read this then it's possible to encode PDFs with Elm!"
                            ]
                        , Pdf.page
                            (Vector2d.fromTuple Length.points ( 400, 100 ))
                            [ Pdf.textBox
                                (Length.points 36)
                                Nothing
                                (Point2d.fromTuple Length.points ( 40, 30 ))
                                "The second page"
                            ]
                        ]
                    )
                    |> Expect.equal expected
        , test "Text containing line breaks" <|
            \_ ->
                let
                    expected =
                        String.replace "\u{000D}\n"
                            "\n"
                            """%PDF-1.7
1 0 obj  % entry point
<< /Title (Multiline text) >>
endobj
2 0 obj
<< /Type /Catalog /Pages 3 0 R >>
endobj
3 0 obj
<< /Kids [ 5 0 R ] /Count 1 /Type /Pages /Resources << /Font << /F1 4 0 R >> /PRocSet [ /PDF /Text ] >> >>
endobj
4 0 obj
<< /Type /Page /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>
endobj
5 0 obj
<< /Type /Page /MediaBox [ 0 0 500.00000 400.00000 ] /Parent 3 0 R /Contents 6 0 R >>
endobj
6 0 obj
<< /Length 178 >>
stream
BT /F1 36.00000 Tf 10.00000 300.00000 Td (If you can read this then) Tj 0 -36.00000 Td ( it's) Tj 0 -36.00000 Td () Tj 0 -36.00000 Td ( possible to encode PDFs with Elm!) Tj ET
endstream
endobj
xref
0 7
0000000000 65535 f
0000000009 00059 n
0000000069 00048 n
0000000118 00121 n
0000000240 00096 n
0000000337 00100 n
0000000438 00226 n
trailer
<< /Size 7 /Info 1 0 R /Root 2 0 R >>
startxref
665
%%EOF"""
                in
                Pdf.encode
                    (Pdf.pdf
                        "Multiline text"
                        [ Pdf.page
                            (Vector2d.fromTuple Length.points ( 500, 400 ))
                            [ Pdf.textBox
                                (Length.points 36)
                                Nothing
                                (Point2d.fromTuple Length.points ( 10, 300 ))
                                "If you can read this then\n it's\n\n possible to encode PDFs with Elm!"
                            ]
                        ]
                    )
                    |> Expect.equal expected
        ]
