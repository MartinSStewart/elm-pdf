module Base exposing (tests)

import Array
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Expect
import Flate
import Hex.Convert
import Parser
import Pdf exposing (Object(..), Operator(..), StreamContent(..))
import Pixels
import Test exposing (Test, describe, test)


jpegImage : Bytes
jpegImage =
    "FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48 00 48 00 00 FF FE 00 13 43 72 65 61 74 65 64 20 77 69 74 68 20 47 49 4D 50 FF DB 00 43 00 03 02 02 03 02 02 03 03 03 03 04 03 03 04 05 08 05 05 04 04 05 0A 07 07 06 08 0C 0A 0C 0C 0B 0A 0B 0B 0D 0E 12 10 0D 0E 11 0E 0B 0B 10 16 10 11 13 14 15 15 15 0C 0F 17 18 16 14 18 12 14 15 14 FF DB 00 43 01 03 04 04 05 04 05 09 05 05 09 14 0D 0B 0D 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 FF C2 00 11 08 00 04 00 05 03 01 11 00 02 11 01 03 11 01 FF C4 00 14 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 08 FF C4 00 14 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 0C 03 01 00 02 10 03 10 00 00 01 54 9F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 05 02 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 03 01 01 3F 01 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 02 01 01 3F 01 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 06 3F 02 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 3F 21 7F FF DA 00 0C 03 01 00 02 00 03 00 00 00 10 9F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 03 01 01 3F 10 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 02 01 01 3F 10 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 3F 10 7F FF D9"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


tests : Test
tests =
    describe "tests"
        [ test "Get jpeg dimensions" <|
            \_ ->
                case Pdf.jpeg "ImageId" jpegImage of
                    Just jpeg ->
                        Pdf.imageSize jpeg |> Expect.equal ( Pixels.pixels 5, Pixels.pixels 4 )

                    Nothing ->
                        Expect.fail "Failed to parse jpeg."
        , test "Decode" <|
            \() ->
                let
                    text =
                        "33 0 obj<</Type/Page/Parent 2 0 R/MediaBox[0 0 595.2756 841.8898]/Contents 34 0 R/Resources<</Font<</F1 4 0 R>>>>>>endobj"

                    bytes2 =
                        BE.string text |> BE.encode
                in
                Parser.run (Pdf.topLevelObjectParser bytes2 text) text
                    |> Expect.equal
                        (Ok
                            ( { index = 33, revision = 0 }
                            , PdfDict
                                [ ( "Type", Name "Page" )
                                , ( "Parent", IndirectReference { index = 2, revision = 0 } )
                                , ( "MediaBox"
                                  , PdfArray (Array.fromList [ PdfInt 0, PdfInt 0, PdfFloat 595.2756, PdfFloat 841.8898 ])
                                  )
                                , ( "Contents", IndirectReference { index = 34, revision = 0 } )
                                , ( "Resources"
                                  , PdfDict
                                        [ ( "Font"
                                          , PdfDict
                                                [ ( "F1"
                                                  , IndirectReference { index = 4, revision = 0 }
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                            )
                        )
        , test "Decode2" <|
            \() ->
                let
                    text =
                        BD.decode (BD.string 303) exampleTopLevelObject |> Maybe.withDefault ""
                in
                Parser.run (Pdf.topLevelObjectParser exampleTopLevelObject text) text
                    |> Expect.equal
                        (Ok
                            ( { index = 34, revision = 0 }
                            , Stream
                                [ ( "Filter", Name "FlateDecode" ), ( "Length", PdfInt 236 ) ]
                                (DrawingInstructions
                                    [ { operator = BT, parameters = [] }
                                    , { operator = Tr, parameters = [ PdfInt 0 ] }
                                    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
                                    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
                                    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 815.6863 ] }
                                    , { operator = Tj, parameters = [ Text "Energy for lighting                                                                         348.2015                3.0700             1068.9786 (268)" ] }
                                    , { operator = ET, parameters = [] }
                                    , { operator = BT, parameters = [] }
                                    , { operator = Tr, parameters = [ PdfInt 0 ] }
                                    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
                                    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
                                    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 809.2793 ] }
                                    , { operator = Tj, parameters = [ Text "Energy saving/generation technologies" ] }
                                    , { operator = ET, parameters = [] }
                                    , { operator = BT, parameters = [] }
                                    , { operator = Tr, parameters = [ PdfInt 0 ] }
                                    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
                                    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
                                    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 802.8723 ] }
                                    , { operator = Tj, parameters = [ Text "PV Unit                                                                                                                               -5643.0184 (269)" ] }
                                    , { operator = ET, parameters = [] }
                                    , { operator = BT, parameters = [] }
                                    , { operator = Tr, parameters = [ PdfInt 0 ] }
                                    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
                                    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
                                    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 796.4653 ] }
                                    , { operator = Tj, parameters = [ Text "Primary energy kWh/year                                                                                                                8654.9537 (272)" ] }
                                    , { operator = ET, parameters = [] }
                                    , { operator = BT, parameters = [] }
                                    , { operator = Tr, parameters = [ PdfInt 0 ] }
                                    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
                                    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
                                    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 790.0583 ] }
                                    , { operator = Tj, parameters = [ Text "Primary energy kWh/m2/year                                                                                                              124.6034 (273)" ] }
                                    , { operator = ET, parameters = [] }
                                    ]
                                )
                            )
                        )
        ]


zlibBytes =
    "58 85 C5 D1 3D 6B C3 30 10 06 E0 DD BF E2 C6 64 E8 F9 F4 2D AD 81 74 CE A0 B6 4B 16 D3 2A B2 D2 44 06 C5 04 FC EF EB 38 14 5A 4A 8B 29 A1 3D C1 81 6E 38 78 DE 5B F9 8A C0 97 B1 5D 5E 89 F5 3D 03 85 0A FC AE 12 04 96 29 D4 56 0B F0 2F 8B 75 0E 25 0E B0 EB 0A 1C 52 6C FB 94 23 DC AA 84 B4 C8 89 A9 2F 73 24 43 F4 69 C4 48 5B 74 C6 6A D8 2E B8 B6 DB E5 D2 EF AB B5 AF 56 3F 43 C8 21 37 EE 23 E4 D4 9C 47 42 1D C3 F8 6D FA D4 65 E8 C3 73 9B BB 43 17 53 38 CD DC CA D1 1A 3E 6D DD 3C C2 43 4E FD CD 22 F9 5D DD 29 2D C7 D0 98 95 53 3C 6E 66 3C C6 69 94 5A 5D 21 25 1D 9B 32 40 B8 C6 F4 FA D4 D6 43 68 CA 5F 4B AC 56 12 9D 12 E6 02 31 7C 36 84 90 94 FD 0E 72 E4 FF 60 61 5C A2 26 31 5D C4 88 77 C8 1B 20 0D A8 81"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


exampleTopLevelObject : Bytes
exampleTopLevelObject =
    "33 34 20 30 20 6F 62 6A 3C 3C 2F 46 69 6C 74 65 72 2F 46 6C 61 74 65 44 65 63 6F 64 65 2F 4C 65 6E 67 74 68 20 32 33 36 3E 3E 73 74 72 65 61 6D 0A 58 85 C5 D1 3D 6B C3 30 10 06 E0 DD BF E2 C6 64 E8 F9 F4 2D AD 81 74 CE A0 B6 4B 16 D3 2A B2 D2 44 06 C5 04 FC EF EB 38 14 5A 4A 8B 29 A1 3D C1 81 6E 38 78 DE 5B F9 8A C0 97 B1 5D 5E 89 F5 3D 03 85 0A FC AE 12 04 96 29 D4 56 0B F0 2F 8B 75 0E 25 0E B0 EB 0A 1C 52 6C FB 94 23 DC AA 84 B4 C8 89 A9 2F 73 24 43 F4 69 C4 48 5B 74 C6 6A D8 2E B8 B6 DB E5 D2 EF AB B5 AF 56 3F 43 C8 21 37 EE 23 E4 D4 9C 47 42 1D C3 F8 6D FA D4 65 E8 C3 73 9B BB 43 17 53 38 CD DC CA D1 1A 3E 6D DD 3C C2 43 4E FD CD 22 F9 5D DD 29 2D C7 D0 98 95 53 3C 6E 66 3C C6 69 94 5A 5D 21 25 1D 9B 32 40 B8 C6 F4 FA D4 D6 43 68 CA 5F 4B AC 56 12 9D 12 E6 02 31 7C 36 84 90 94 FD 0E 72 E4 FF 60 61 5C A2 26 31 5D C4 88 77 C8 1B 20 0D A8 81 0A 65 6E 64 73 74 72 65 61 6D 0A 65 6E 64 6F 62 6A 0A"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))
