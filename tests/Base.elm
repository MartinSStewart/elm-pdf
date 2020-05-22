module Base exposing (tests)

import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
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
                    expected : String
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
BT /F1 30.00000 Tf 10.00000 30.00000 Td (If you can read this then it's possible to encode PDFs with Elm!) Tj ET
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
                Pdf.pdf
                    "test"
                    [ Pdf.page
                        { size = Vector2d.fromTuple Length.points ( 1100, 100 )
                        , contents =
                            [ Pdf.text
                                (Length.points 30)
                                (Point2d.fromTuple Length.points ( 10, 30 ))
                                "If you can read this then it's possible to encode PDFs with Elm!"
                            ]
                        }
                    ]
                    |> Pdf.encoder
                    |> BE.encode
                    |> (\bytes -> BD.decode (BD.string (Bytes.width bytes)) bytes)
                    |> Expect.equal (Just expected)
        , test "Two page PDF" <|
            \_ ->
                let
                    expected : String
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
                Pdf.pdf
                    "Two pages"
                    [ Pdf.page
                        { size = Vector2d.fromTuple Length.points ( 1100, 100 )
                        , contents =
                            [ Pdf.text
                                (Length.points 36)
                                (Point2d.fromTuple Length.points ( 10, 30 ))
                                "If you can read this then it's possible to encode PDFs with Elm!"
                            ]
                        }
                    , Pdf.page
                        { size = Vector2d.fromTuple Length.points ( 400, 100 )
                        , contents =
                            [ Pdf.text
                                (Length.points 36)
                                (Point2d.fromTuple Length.points ( 40, 30 ))
                                "The second page"
                            ]
                        }
                    ]
                    |> Pdf.encoder
                    |> BE.encode
                    |> (\bytes -> BD.decode (BD.string (Bytes.width bytes)) bytes)
                    |> Expect.equal (Just expected)
        , test "Text containing line breaks" <|
            \_ ->
                let
                    expected : String
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
<< /Type /Page /MediaBox [ 0 0 700.00000 400.00000 ] /Parent 3 0 R /Contents 6 0 R >>
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
                Pdf.pdf
                    "Multiline text"
                    [ Pdf.page
                        { size = Vector2d.fromTuple Length.points ( 700, 400 )
                        , contents =
                            [ Pdf.text
                                (Length.points 36)
                                (Point2d.fromTuple Length.points ( 10, 300 ))
                                "If you can read this then\n it's\n\n possible to encode PDFs with Elm!"
                            ]
                        }
                    ]
                    |> Pdf.encoder
                    |> BE.encode
                    |> (\bytes -> BD.decode (BD.string (Bytes.width bytes)) bytes)
                    |> Expect.equal (Just expected)
        , test "Text containing unicode" <|
            \_ ->
                let
                    expected : String
                    expected =
                        String.replace "\u{000D}\n"
                            "\n"
                            """%PDF-1.7
1 0 obj  % entry point
<< /Title (Unicode text) >>
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
<< /Type /Page /MediaBox [ 0 0 700.00000 400.00000 ] /Parent 3 0 R /Contents 6 0 R >>
endobj
6 0 obj
<< /Length 82 >>
stream
BT /F1 36.00000 Tf 10.00000 300.00000 Td (Unicode support ဍᄍ」䀍倍) Tj ET
endstream
endobj
xref
0 7
0000000000 65535 f
0000000009 00057 n
0000000067 00048 n
0000000116 00121 n
0000000238 00096 n
0000000335 00100 n
0000000436 00129 n
trailer
<< /Size 7 /Info 1 0 R /Root 2 0 R >>
startxref
566
%%EOF"""
                in
                Pdf.pdf
                    "Unicode text"
                    [ Pdf.page
                        { size = Vector2d.fromTuple Length.points ( 700, 400 )
                        , contents =
                            [ Pdf.text
                                (Length.points 36)
                                (Point2d.fromTuple Length.points ( 10, 300 ))
                                "Unicode support ဍᄍ」䀍倍"
                            ]
                        }
                    ]
                    |> Pdf.encoder
                    |> BE.encode
                    |> (\bytes -> BD.decode (BD.string (Bytes.width bytes)) bytes)
                    |> Expect.equal (Just expected)
        ]



--utf8Decoder : Int -> BD.Decoder String
--utf8Decoder byteLength =
--    BD.loop ( 0, [] )
--        (\( counter, text ) ->
--            if counter >= byteLength then
--                BD.succeed (BD.Done text)
--
--            else
--                BD.unsignedInt8 |> BD.map (\value -> BD.Loop ( counter + 1, Char.fromCode value :: text ))
--        )
--        |> BD.map (List.reverse >> String.fromList)
