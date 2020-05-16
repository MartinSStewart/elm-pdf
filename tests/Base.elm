module Base exposing (tests)

import Expect exposing (Expectation)
import Length
import Pdf
import Test exposing (Test, describe, test)
import Vector2d


tests =
    describe "tests"
        [ test "generate hallo wald" <|
            \_ ->
                let
                    expected =
                        """%PDF-1.4
1 0 obj
<< /Title (Hallo Welt) >>
endobj
2 0 obj
<< /Type /Catalog
   /Pages 3 0 R
>>
endobj
3 0 obj
<< /Type /Pages
   /MediaBox [0 0 595 842]
   /Resources
   << /Font << /F1 4 0 R >>
      /ProcSet [/PDF /Text]
   >>
   /Kids [5 0 R]
   /Count 1
>>
endobj
4 0 obj
<< /Type /Font
   /Subtype /Type1
   /BaseFont /Helvetica
   /Encoding /WinAnsiEncoding
>>
endobj
5 0 obj
<< /Type /Page
   /Parent 3 0 R
   /Contents 6 0 R
>>
endobj
6 0 obj
<< /Length 41
>>
stream
/F1 48 Tf
BT
72 746 Td
(Hallo Welt) Tj
ET
endstream
endobj
xref
0 7
0000000000 65535 f
0000000009 00000 n
0000000050 00000 n
0000000102 00000 n
0000000268 00000 n
0000000374 00000 n
0000000443 00000 n
trailer
<< /Size 7
   /Info 1 0 R
   /Root 2 0 R
>>
startxref
534
%%EOF"""
                in
                Pdf.encode
                    (Pdf.init
                        { title = "test"
                        , firstPage = Pdf.page (Vector2d.fromTuple Length.points ( 100, 100 )) []
                        }
                    )
                    |> Debug.log "a"
                    |> Expect.equal expected
        ]
