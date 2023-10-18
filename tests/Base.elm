module Base exposing (tests)

import Array
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict
import Expect
import Flate
import Hex.Convert
import Parser
import Pdf exposing (GraphicsInstruction, Object(..), Operator(..), StreamContent(..))
import Pixels
import Rc4
import Rc4_2
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
                Parser.run (Pdf.topLevelObjectParser Nothing bytes2 text) text
                    |> Expect.equal
                        (Ok
                            ([ ( "Type", Name "Page" )
                             , ( "Parent", IndirectReference { index = 2, revision = 0 } )
                             , ( "MediaBox"
                               , PdfArray (Array.fromList [ PdfInt 0, PdfInt 0, PdfFloat 595.2756, PdfFloat 841.8898 ])
                               )
                             , ( "Contents", IndirectReference { index = 34, revision = 0 } )
                             , ( "Resources"
                               , [ ( "Font"
                                   , [ ( "F1", IndirectReference { index = 4, revision = 0 } ) ]
                                        |> Dict.fromList
                                        |> PdfDict
                                   )
                                 ]
                                    |> Dict.fromList
                                    |> PdfDict
                               )
                             ]
                                |> Dict.fromList
                                |> PdfDict
                            )
                        )
        , test "Decode2" <|
            \() ->
                let
                    text =
                        BD.decode (BD.string 303) exampleTopLevelObject |> Maybe.withDefault ""
                in
                Parser.run (Pdf.topLevelObjectParser Nothing exampleTopLevelObject text) text
                    |> Expect.equal
                        (Ok
                            (Stream
                                (Dict.fromList [ ( "Filter", Name "FlateDecode" ), ( "Length", PdfInt 236 ) ])
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
        , test "Decode3" <|
            \() ->
                let
                    text =
                        BD.decode
                            (BD.string (Bytes.width exampleTopLevelObject2))
                            exampleTopLevelObject2
                            |> Maybe.withDefault ""
                in
                Parser.run (Pdf.topLevelObjectParser Nothing exampleTopLevelObject2 text) text
                    |> Expect.equal
                        (Ok
                            (Stream
                                (Dict.fromList [ ( "Filter", Name "FlateDecode" ), ( "Length", PdfInt 3153 ) ])
                                (DrawingInstructions expectedDrawingInstructions)
                            )
                        )
        , test "RC4 encode" <|
            \() ->
                let
                    key =
                        "Wiki"

                    plainText =
                        "pedia"
                in
                Rc4.encrypt key plainText
                    |> Expect.equal [ 16, 33, 191, 4, 32 ]
        , test "RC4 decode" <|
            \() ->
                let
                    key =
                        "Wiki"

                    plainText =
                        [ 16, 33, 191, 4, 32 ]
                in
                Rc4.decrypt key plainText
                    |> Expect.equal "pedia"
        , test "Encrypted stream" <|
            \() ->
                let
                    ownerHash =
                        "47 E3 00 72 EF 8A 45 6C B2 09 4A 62 69 AE 78 1C 7F 43 53 4C A2 7B 65 8B 13 54 F3 DC 3F 5C 5C 69 A7"
                            |> String.filter Char.isAlphaNum
                            |> Hex.Convert.toBytes
                            |> Maybe.withDefault (BE.encode (BE.sequence []))

                    userHash =
                        "98 06 D8 E0 96 5D 80 2D F1 AF 6B 68 B8 D7 B8 20 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
                            |> String.filter Char.isAlphaNum
                            |> Hex.Convert.toBytes
                            |> Maybe.withDefault (BE.encode (BE.sequence []))

                    idEntry =
                        "d19ade06181e7ee412ba31589d95c0bed19ade06181e7ee412ba31589d95c0be"
                            |> String.filter Char.isAlphaNum
                            |> Hex.Convert.toBytes
                            |> Maybe.withDefault (BE.encode (BE.sequence []))

                    pEntry =
                        "2D 31 38 35 32"
                            |> String.filter Char.isAlphaNum
                            |> Hex.Convert.toBytes
                            |> Maybe.withDefault (BE.encode (BE.sequence []))

                    key =
                        Pdf.getRc4Key 128 ownerHash Pdf.defaultPassword pEntry idEntry
                in
                Rc4_2.decrypt key encryptedStream
                    |> Hex.Convert.toString
                    |> Expect.equal ""
        ]


encryptedStream : Bytes
encryptedStream =
    "69 CE EE 60 AA EF AB 66 BB 44 F6 D0 48 4F E8 D7 9A 65 D8 9C 9A 1B 97 60 B1 DC C7 98 4E 53 73 3E 77 5C 8F 1D C0 10 B8 09 86 65 EF 1F 51 29 01 B3 C1 7E 4B 39 7A E0 F8 31 02 30 E9 EF 24 62 45 D5 06 01 81 9D 5F 1C A2 2B 06 2F F0 E7 A2 E6 E5 E5 37 01 4E 07 5D 9F 64 DC C7 7C 59 82 03 D4 11 3E 08 69 28 C1 E9 3E 75 F5 1A 25 43 F4 61 0E 3D A1 67 69 E2 FF AC 37 1A B9 23 9E 59 80 F3 1B C5 B7 E6 35 CA 2F C9 E9 BB D9 3A 56 90 5A 4B EE 0C 7C 0D 0B 3F 8C 79 75 0C D1 A6 4A A4 44 7A 0E 52 30 8B AB 38 41 E8 5F CA B6 87 B1 B9 0F 97 71 AC 6D 22 BC C0 A0 2D CF 08 A6 4B B4 CA 2E 9F 73 F9 C2 1E F2 5C 45 52 63 F9 90 BE 51 04 65 A1 8C 7A 4A 93 81 82 B5 CD D0 46 7C 27 C7 81 4E B8 A3 10 AC 5C 09 35 EF DE 64 73 35 C3 27 E6 E0 45 83 ED 2C D6 42 98 32 F0 95 E7 9B 53 ED 2C F9 46 C5 8B 90 D5 0B 22 C4 6F DB 6F 84 CC 13 E4 80 41 59 44 FB D0 B4 E0 98 F9 70 77 4B 8D AB F9 AF 08 75 41 40 86 DF 0F C8 27 C5 52 74 B7 83 D4 F6 CC AF 33 25 97 6D 1D CE 4E 05 F1 A0 FD 07 E8 B4 C1 A1 C9 82 C0 6F 9D D5 BB 75 8B FA A6 4C 45 AF 01 7D C8 A2 F6 42 F9 D7"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


exampleTopLevelObject : Bytes
exampleTopLevelObject =
    "33 34 20 30 20 6F 62 6A 3C 3C 2F 46 69 6C 74 65 72 2F 46 6C 61 74 65 44 65 63 6F 64 65 2F 4C 65 6E 67 74 68 20 32 33 36 3E 3E 73 74 72 65 61 6D 0A 58 85 C5 D1 3D 6B C3 30 10 06 E0 DD BF E2 C6 64 E8 F9 F4 2D AD 81 74 CE A0 B6 4B 16 D3 2A B2 D2 44 06 C5 04 FC EF EB 38 14 5A 4A 8B 29 A1 3D C1 81 6E 38 78 DE 5B F9 8A C0 97 B1 5D 5E 89 F5 3D 03 85 0A FC AE 12 04 96 29 D4 56 0B F0 2F 8B 75 0E 25 0E B0 EB 0A 1C 52 6C FB 94 23 DC AA 84 B4 C8 89 A9 2F 73 24 43 F4 69 C4 48 5B 74 C6 6A D8 2E B8 B6 DB E5 D2 EF AB B5 AF 56 3F 43 C8 21 37 EE 23 E4 D4 9C 47 42 1D C3 F8 6D FA D4 65 E8 C3 73 9B BB 43 17 53 38 CD DC CA D1 1A 3E 6D DD 3C C2 43 4E FD CD 22 F9 5D DD 29 2D C7 D0 98 95 53 3C 6E 66 3C C6 69 94 5A 5D 21 25 1D 9B 32 40 B8 C6 F4 FA D4 D6 43 68 CA 5F 4B AC 56 12 9D 12 E6 02 31 7C 36 84 90 94 FD 0E 72 E4 FF 60 61 5C A2 26 31 5D C4 88 77 C8 1B 20 0D A8 81 0A 65 6E 64 73 74 72 65 61 6D 0A 65 6E 64 6F 62 6A 0A"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


exampleTopLevelObject2 : Bytes
exampleTopLevelObject2 =
    "32 30 20 30 20 6F 62 6A 3C 3C 2F 46 69 6C 74 65 72 2F 46 6C 61 74 65 44 65 63 6F 64 65 2F 4C 65 6E 67 74 68 20 33 31 35 33 3E 3E 73 74 72 65 61 6D 0A 58 85 DD 5C D9 72 DB 38 16 7D CF 57 E0 65 AA 9C EA 08 C1 BE F4 9B E3 76 3A DD 13 27 4E E2 4E 66 A6 FC 42 53 B4 CC 89 44 AA 48 CA CB FC 8E 7F 74 2E 00 2D 94 97 34 48 D9 72 CA 48 55 4A 9B 85 73 2E 80 7B CF 05 2E F4 E6 E8 05 41 47 15 FC E7 FE 55 A3 D7 6F 29 92 58 A2 A3 D3 17 9C 20 43 25 56 46 71 74 34 DC F9 32 4D D2 0C 9D 65 49 93 17 23 74 3A CB C6 68 80 EA 2C 2D 8B 61 52 5D A1 2D 36 82 09 34 74 BC C3 A8 3C 7E F9 F2 E8 BF 2F F6 8F 5E BC F9 31 11 62 31 D3 D6 13 D9 1F 67 69 53 E5 69 DE 5C A1 D3 B2 42 D3 D9 64 5A A3 A4 18 A2 D3 A4 A8 7F 8D FC 3E 86 8D 66 FE FB 10 4A B3 A2 A9 92 F1 D2 38 EE 1B B7 69 10 DF 28 5B D9 85 93 34 D2 30 DA 2A 2C 94 9C 13 99 24 79 B1 1A E2 F1 2C 73 26 D9 3A 13 21 5B 44 B2 68 22 40 5F 1A 4F E4 A8 6C 60 34 B2 1B E3 DC 9C 65 28 39 29 CF B3 57 E8 FB B7 B3 D7 57 59 52 3D 32 11 AA 5A 4C 68 2C 11 C3 B1 92 54 DC 35 55 C7 F9 E8 CC 8F CD F1 4E 9A 8C D3 D9 38 69 B2 21 82 31 DB 9D 4E B3 62 98 5F A2 F7 C7 2F 1F 85 0A 17 06 33 42 A5 A7 C2 62 A9 68 8D 99 10 81 4A 91 55 A3 2B 54 27 E7 80 FF F5 28 83 A7 30 CD CA 02 35 59 7A 56 94 E3 72 94 67 35 7C FB 9C 49 0A 4F 0E 5E A1 0F 7E 5D 7E 8A EE 8F 60 C3 B5 EF EF F0 2B 6A 75 F2 94 AD B5 2C 79 2C 11 25 B0 E0 C4 13 F9 96 83 05 7E 0A 2A 2D 22 22 96 88 34 98 C0 C7 1D 91 77 57 C3 AA 1C 2C 56 65 9B D2 72 D4 2F D1 87 47 9A BF F7 53 91 49 34 17 8A 15 55 B7 16 E6 9C 08 2C C4 01 3A C8 D3 AA 44 7B EF 0E B7 C8 A9 CD 25 96 0A B8 58 46 AC 74 54 96 30 3F B9 78 3E CD D2 1C 7C E7 29 C4 80 59 95 D5 71 DF C6 0D 86 80 28 43 0C C9 96 0B 1D 2C 02 3E 6B 65 9E AD B6 96 55 54 AC 55 38 C3 DC CA 75 1E B3 7A DB C8 EF E5 A1 63 79 30 89 AD 31 72 15 0A 87 D9 38 3F CF 2A 60 32 67 E5 82 49 32 1E 3B 76 F5 16 48 50 4D 0C 78 34 C1 B9 27 62 62 89 50 8B 25 68 50 47 64 F0 33 B7 48 36 1C 53 2D 94 63 43 49 82 D1 5B A7 A1 D3 B2 6E 6A 58 79 B3 DA C5 F5 37 FB BF BD 45 D3 CA 87 BF E3 1D C9 A2 87 9C 28 AC 95 56 CF C4 52 84 60 AE 88 0A 0B F1 51 9B 1F 83 9B CF FD 00 DC FE 98 1B AA 28 FC CA 72 6C 25 7F 6C FC F7 AA D8 E9 6B 78 EB E6 8B D7 FE B3 71 F8 8D C6 52 28 75 3B ED 1B 84 14 A1 BE AA 9B 6C 82 E8 46 F0 29 97 90 7C 28 6B EC CD 37 08 06 E1 4F 6E 7C 18 F4 BD 15 CC 3B 10 41 22 97 85 32 14 C3 5F 7A 22 DF 20 06 55 4B 22 C7 3B 25 E4 03 95 CF 63 1F 20 3C 33 AB 05 98 8C C5 50 41 20 EC 30 B3 52 7B 2A B1 2B 5C 41 07 10 A0 B4 D7 B6 6B 19 AB 77 E6 0B 5E 1B B7 65 CE 72 03 33 F8 17 76 EB 55 45 C0 3F 1B 1F 9D 84 8D 25 A2 0C E6 4C EA 56 52 B0 96 DA 3C 54 5B 66 2C 11 44 28 D3 D8 C0 6C 74 44 64 F4 E4 52 14 5B 6A 3C 91 DD E1 30 77 52 16 62 6D DD C0 C0 38 22 E9 59 52 8D B6 12 5E 5B 44 08 5F EA 05 19 9B 71 2A 09 8F 29 D5 AD D4 39 8C 8B 73 78 5B 85 BF E2 61 0D B8 06 23 C3 88 C4 CA 5A 25 2C A6 44 E8 E7 11 05 95 60 E0 1D B5 67 43 19 E8 85 BD A4 3A 81 6C 69 98 97 97 F9 30 43 D9 24 AF 6B 98 73 4E 3D FC 01 53 EE 3C 1F CE 5A 9B 50 C1 47 D7 28 2F D2 F1 CC 4F C8 89 4B 4E 06 90 9C C4 75 0F 2B 85 59 62 9E 89 31 99 C5 C6 70 F3 E8 92 62 EE D2 E6 4F E6 43 04 7E 3A 6D CA F5 40 BD 78 2F 2E C7 52 8C 63 A1 D5 63 E3 BF 4B 52 7C 1F A1 BD 8F EC 86 A4 98 BF 18 2F 29 A8 C6 04 62 FD 13 49 0A 48 A3 18 55 EB 5E 9F 59 98 E1 C6 68 EF 2C 55 B4 B3 84 90 AE 14 33 4F 26 29 EE A2 E2 7D 85 24 D4 AB 23 15 BB 39 A3 9C 0E 91 D2 AE C6 C4 49 8A 8B 35 52 5B 6E 5C 6A 8B B9 31 CC 13 89 F5 FB D2 42 F8 16 C6 DE A1 8D 1E B4 DD A3 8D 60 44 24 B5 37 5F 35 12 2B 2E C3 E4 8A 15 79 D2 52 C8 92 A9 7D 22 6D 74 27 11 6A 08 D6 54 F9 48 AC 62 33 77 09 EC 09 17 76 25 29 BE 8F B6 B1 E3 FE 83 C6 8D 90 A0 A0 C1 05 01 11 1D BB 85 2D B5 C1 8A 69 FB 3C A2 A0 D4 0C 33 46 3C 1B CA 41 52 1C 56 F9 C4 9D DE CD 05 DF A3 08 09 09 AB C6 84 F3 8C A7 36 D2 0F 5B 1C 19 69 B1 20 7A 5B 32 E2 C6 F8 AC CB 88 F5 37 23 E1 F3 9F 45 41 B4 3E 1B 07 5D 28 2C ED 1D 47 D1 DB 11 10 D4 2D 9C 35 D7 08 D1 C0 68 6C 35 53 9D 04 84 14 04 53 F3 74 5B 12 77 30 81 A4 98 19 B7 BB 22 3A E9 07 C9 39 D6 FA 27 92 0F 8C 80 32 82 9C 7C 1E 75 A3 E5 03 64 FF 5C 33 FA 54 F2 01 52 76 7D F3 55 49 60 B6 4B D2 51 3E 30 82 AD 92 F4 A9 E4 C3 1D 44 28 51 30 B3 B4 E9 28 1F 28 4C 5D 69 C2 88 AC BB C0 2D 1D DD DF 6C 8C 6A CB DC 09 86 EA 26 1F 88 C1 54 D2 FB 88 4C D8 13 70 E1 14 96 AD 00 11 E0 88 C4 9E 48 43 72 01 7F 24 E8 F3 88 E2 C2 4A CC 61 C9 3F 13 36 06 96 18 27 9E CD 97 DD 43 70 83 94 A1 FD C3 3D F4 C7 C1 E1 E7 8F 5F F7 0F F6 3F 1C 7D 81 D1 FE 9A 55 7E 33 C2 82 AB 7F 85 76 67 A3 59 DD B8 0F C7 EE DC 0A 03 49 26 E3 CF C5 6A 5A 61 4A 95 67 B3 37 AB AA AC 68 16 4B 33 3B 3D CD D3 3C 2B D2 2B 54 F9 38 F6 6B E4 D2 FA 0D 72 EA B8 CE 95 C5 9A D8 1B 9D 9F E7 55 59 4C E0 31 A8 EF 7C 02 21 B5 E9 D2 FF 3E 48 8B C8 CE 39 86 E9 E2 3B 3F DE 79 EB 4A A3 B2 7A 5E B8 56 4D CB 3A AB 8F 5F C6 95 C1 09 A9 B0 B1 41 53 EE 22 F4 BE 3C 6D 20 4D A8 5D 5D D2 26 A5 2A FB 97 79 C0 33 2B BE 17 E5 45 11 89 85 60 58 09 01 0B 43 6F 01 03 AA CA F2 74 43 40 1F CA 06 25 D3 E9 38 4F 93 93 71 16 87 44 08 4C 0C 0D 48 38 FA 5C 96 93 41 5E 0C 36 05 D3 07 09 D7 58 69 E1 91 BC 41 68 2F 39 77 05 2B 17 EE E4 7F 13 AB F4 42 02 B2 53 E9 80 44 A0 C3 A4 7A 08 20 BD 90 30 81 8D 22 1E C9 27 04 09 2F E8 55 77 6E 03 CB 20 BB 9C 3F EE 01 EC 73 96 96 13 58 BB C3 6C 18 07 83 1A 2C 64 A8 8C FA 04 EE 7A D1 B5 93 9E 69 FF 71 EA 63 10 CA 30 11 A1 AE E9 1B 85 85 53 82 29 5A DD 1E EF D4 B3 7A EA 99 A1 53 F7 66 6C 72 D2 D9 24 C4 6D 99 D9 00 84 DD 09 A4 1C E7 1D 41 F4 34 0A B7 16 43 A8 F3 58 F6 10 7A 07 DF 10 32 9B F4 6A 9C 03 A3 AA DF BC DD 1D 57 59 32 BC 72 7F 0C 4E 7E 1C 69 18 0E D2 D3 30 E9 C1 FC 06 51 A6 4A 66 A0 E8 D1 D4 79 94 0D 74 7D 4F 30 46 61 98 BC BE 28 67 DF F9 FC 8B 45 D8 DC 28 CF 70 C2 25 2F 52 40 54 67 A8 29 4B 54 4F 00 53 24 22 82 09 0D 65 42 6F C1 CB 2D 46 C8 65 D3 93 12 B8 F5 3B C4 EC 69 1E CD B1 22 C2 83 F9 1D 26 CE 3C 11 4E CB A2 A9 CA 71 EF 8C B2 27 18 A5 31 B5 DA 83 79 F7 70 60 FA AC 27 45 B0 B6 C4 23 F9 13 A1 37 79 39 49 EA 1A 9D 94 F9 38 DB 20 FF E9 83 44 0A CC 0D F7 48 FE B9 42 52 37 E5 79 86 2E F2 E6 AC 1F A8 3E 48 84 DB 3E 0A 65 36 7F B2 9B 36 39 DE 49 C6 3E 26 34 F9 79 D6 6D 3F A8 17 16 8A A5 0A 95 32 FF A1 68 37 AF 7C ED 66 55 CE 20 1E D5 E5 AC 9A 6F B8 85 BB 0D 1D B0 F5 C1 C2 25 06 61 1E B0 B0 BF C3 E2 87 6C E6 16 BC 8F 0B AD ED B4 5B 18 FB 60 61 06 6B 29 03 16 BE 56 DF DB 7B 78 7A DB 85 31 CC 45 D8 3A FC 03 FC 5C 09 A4 0B 5F 36 B8 D1 32 EA E9 5A A8 84 34 92 7A 30 9F D7 C0 00 96 FE 80 FA 98 85 58 0C 42 CA EF C9 7F 79 38 B3 F4 42 C2 31 65 A1 62 E3 08 A1 DF 93 DA B9 DA 8D D1 F4 40 C2 AC 2F 73 0A 48 9C 94 9A 65 68 04 70 FC B2 A9 40 97 9D 67 7D EE 6C F5 42 42 30 A7 DC 23 79 CF D0 3B D0 08 0B 14 0D 64 BA 4E 3E 81 EB AD 92 51 58 D3 59 15 17 95 FA 20 31 1C 5B A2 3C 92 03 A7 4E A7 E3 24 CD 5C BA 0D D2 AE 9A A0 04 FC CC AC C8 BB 8A 85 3E 48 B4 C6 C2 5A 8F E4 03 48 9F 72 9C 54 0F B0 71 DE 55 6F 33 4D 31 B1 A1 94 E1 DF 41 1D AC E6 C6 FC 38 C5 ED 1F 4F F2 4B 40 56 9F 95 17 51 A3 D3 C7 20 4A B8 6A 2F 8F E4 23 08 DC 72 06 7F 89 46 E3 E4 7F 90 73 5C 80 9A 2B 2F FA 88 95 7E 9E 8D 49 83 99 36 01 0C 5F 9B 27 0E 51 D8 97 C8 9B AE 80 FE 82 7C 21 6F 92 05 31 F8 9A 48 34 14 1B 4D 3D 9A 43 98 2B CB 0B 97 0B 2C FD 5A 9F 41 12 12 43 9A E8 91 FC 0B 85 95 3C 85 A8 5B 56 93 A4 80 90 BC CC 9E 87 10 86 E3 AD B3 81 F0 67 DC 62 22 C3 09 D4 5F 8B 75 34 3D 2B 41 CA 95 E3 26 C9 53 34 4D 8A AC B3 CE ED BC 8E 38 C3 4A 86 53 F5 AF 0C F9 0B 5B CD AC 3A C9 8B EC EF 3B BB BF F5 19 22 A6 30 13 DC 23 69 B1 40 13 30 AE BB CE 73 F7 3E A2 1B 81 F4 2C 29 46 0E EE 9E AB F9 5C 3E 43 EE E0 76 FE 34 0E 01 B5 D8 70 65 3B EE AE FC 82 C0 23 2E F0 0C AE DD EE C8 0A DF C0 ED EA 7D 1F F9 AB 23 98 FC 23 72 B7 9A 51 8E 05 B3 B6 F3 B6 C6 2F 88 62 DD C2 82 6C EB A0 6B E0 8E 4E 3D 16 85 4D 34 14 A2 31 61 CC 76 F4 F7 0E C8 F2 F4 16 80 50 DA 3A 5B 1B C0 72 0C 40 0C B6 F1 40 88 BB C2 D6 71 BD 00 8E 95 45 00 87 D2 6D 83 58 29 02 0E AE E3 81 50 2B F0 BC 0E E5 60 3E 37 51 39 C9 9B 70 8B AE 35 21 61 CC 7C 15 72 B8 37 BA F2 0F 71 FB D3 D4 68 77 33 AD E3 56 C5 2F AE C4 AE 3D 01 68 AB A8 60 40 E7 66 27 58 46 B3 35 D4 5D 2C EB E8 38 1D 0C D9 86 01 19 D1 0A 06 A4 24 61 49 60 16 0D 43 0B 77 2F EC E1 0A 40 16 B5 AE FB ED 93 8B 38 28 CA E0 F9 F9 6E 74 6F 47 57 53 70 85 6E 87 B4 70 E5 49 61 46 2C CC D5 3A B2 99 1F 9D 44 E2 60 EE 4E 16 BD CF 61 C6 7D 89 94 EE F2 95 FF 92 F8 ED E4 EB 35 1F E7 EE 20 80 62 77 65 72 13 D7 29 C5 94 B8 8A FC DA 75 25 20 AB 70 9D ED 30 F7 16 99 BF 1A 00 EC F8 53 A7 B0 3A E1 41 70 A2 A3 2A CB E2 4E 4D A8 B0 EE AA 95 C7 DE 65 E7 F7 7A CD 2B 42 63 18 66 56 1F F4 7B 48 D9 05 7A DD 19 3D 73 17 AD 3C FA 78 15 7D BD E6 48 91 AB 5B 90 FD 2C BF 87 74 70 0D F0 20 7C 25 64 32 B1 A1 92 72 E5 6E 59 B5 C0 C7 48 97 EB 35 EF EB 2A 96 B0 E6 BD C0 BF 41 10 56 03 78 43 BA 1A 9E 13 77 B1 8A FD 78 01 87 CA CF 2F 8B 85 7A 4D 99 6D 4F 79 90 4D 46 76 87 1E 81 8E 39 A3 84 03 CE C3 D2 A7 99 AB 4B 2D FD 0E 77 9D B1 E2 2C 43 B5 BB DE 74 AB EF CD CE 76 D1 72 A0 22 41 10 77 35 C9 83 58 DD 68 0C 37 C5 7C 38 1D 26 4D 82 DC 8F 2F 9C E5 35 CC 34 08 B4 A7 55 39 F1 2F 9F 38 1D 5E 65 E7 B9 AF 15 28 66 93 13 58 50 D2 5D 3C DB FF 72 E4 6B 46 20 09 99 BA 5A AB D8 DA 11 4A 84 BB 5F C4 D6 9C EC E2 C7 1F 2E 9B 50 6E 95 0C 87 EE D5 5B 3D 2B 4C A1 53 88 C0 7F CE 8A 2E C5 0A D6 B8 AB 40 BE CF 45 E0 58 78 05 D7 DD 52 00 84 FB B7 E5 69 30 C5 59 39 C9 A0 3B 88 CE C8 15 E6 BC 42 47 67 C9 04 0C F7 D5 A5 8F 57 B1 C7 E2 96 BA 8B 3B 5D 2A 2D 17 35 00 F3 B6 9A 38 BE 85 15 14 F7 C3 34 D2 5D DA 09 5D B7 7F A4 E0 DE 76 4D CD 6A 4D 5E BB 2D A3 45 1B C0 5B 91 BF 58 E2 AE D6 84 3E 0F 12 88 1F 7E AB E9 07 ED 9A EA 95 23 00 6F 2C 97 9D 5E 2B 41 23 EF B8 BB EB 30 A1 CF F5 72 D0 7B FB 94 52 AE 9E B8 5B 62 8B 27 92 C4 F1 54 CA 5D 61 09 7D 7E 8B D9 AD B9 76 3F E1 D1 32 B4 6D 3D A1 32 F2 46 80 BB 6D 12 FA 7C 1F 73 BE 06 4E D6 DC F3 64 EE 3B FE 0F A4 BA B0 78 0A 65 6E 64 73 74 72 65 61 6D 0A 65 6E 64 6F 62 6A 0A"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


expectedDrawingInstructions : List GraphicsInstruction
expectedDrawingInstructions =
    [ { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 815.6863 ] }
    , { operator = Tj, parameters = [ Text "Space heating fuel - secondary                                                                                                            0.0000 (215)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 809.2793 ] }
    , { operator = Tj, parameters = [ Text "Electricity for pumps and fans:" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 802.8723 ] }
    , { operator = Tj, parameters = [ Text "  central heating pump                                                                                                                 120.0000 (230c)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 796.4653 ] }
    , { operator = Tj, parameters = [ Text "  main heating flue fan                                                                                                                 45.0000 (230e)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 790.0583 ] }
    , { operator = Tj, parameters = [ Text "Total electricity for the above, kWh/year                                                                                               165.0000 (231)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 783.6514 ] }
    , { operator = Tj, parameters = [ Text "Electricity for lighting (calculated in Appendix L)                                                                                     348.2015 (232)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 777.2444 ] }
    , { operator = Tj, parameters = [ Text "Energy saving/generation technologies (Appendices M, N and Q)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 770.8374 ] }
    , { operator = Tj, parameters = [ Text "PV generation                                                                                                                             0.0000 (233)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 764.4304 ] }
    , { operator = Tj, parameters = [ Text "Wind generation                                                                                                                           0.0000 (234)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 758.0234 ] }
    , { operator = Tj, parameters = [ Text "Hydro-electric generation (Appendix N)                                                                                                    0.0000 (235a)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 751.6164 ] }
    , { operator = Tj, parameters = [ Text "Electricity generated - Micro CHP (Appendix N)                                                                                            0.0000 (235)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 745.2095 ] }
    , { operator = Tj, parameters = [ Text "Appendix Q - special features" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 738.8025 ] }
    , { operator = Tj, parameters = [ Text "  energy saved or generated                                                                                                              0.0000 (236)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 732.3955 ] }
    , { operator = Tj, parameters = [ Text "  energy used                                                                                                                            0.0000 (237)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 725.9885 ] }
    , { operator = Tj, parameters = [ Text "Total delivered energy for all uses                                                                                                   17084.4433 (238)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 719.5815 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 713.1746 ] }
    , { operator = Tj, parameters = [ Text "10a. Fuel costs - using BEDF prices (527)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 706.7676 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 700.3606 ] }
    , { operator = Tj, parameters = [ Text "                                                                                               Fuel            Fuel price             Fuel cost" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 693.9536 ] }
    , { operator = Tj, parameters = [ Text "                                                                                           kWh/year                 p/kWh                £/year" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 687.5466 ] }
    , { operator = Tj, parameters = [ Text "Space heating - main system 1                                                             13596.6989               10.2300             1390.9423 (240)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 681.1396 ] }
    , { operator = Tj, parameters = [ Text "Water heating (other fuel)                                                                 2974.5429               10.2300              304.2957 (247)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 674.7327 ] }
    , { operator = Tj, parameters = [ Text "Pumps and fans for heating                                                                  165.0000               36.7200               60.5880 (249)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 668.3257 ] }
    , { operator = Tj, parameters = [ Text "Energy for lighting                                                                         348.2015               36.7200              127.8596 (250)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 661.9187 ] }
    , { operator = Tj, parameters = [ Text "Additional standing charges                                                                                                             103.0000 (251)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 655.5117 ] }
    , { operator = Tj, parameters = [ Text "Total energy cost                                                                                                                      1986.6856 (255)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 649.1047 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 642.6977 ] }
    , { operator = Tj, parameters = [ Text "12a. Carbon dioxide emissions - Individual heating systems including micro-CHP" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 636.2908 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 629.8838 ] }
    , { operator = Tj, parameters = [ Text "                                                                                             Energy       Emission factor             Emissions" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 623.4768 ] }
    , { operator = Tj, parameters = [ Text "                                                                                           kWh/year            kg CO2/kWh           kg CO2/year" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 617.0698 ] }
    , { operator = Tj, parameters = [ Text "Space heating - main system 1                                                             13596.6989                0.2160             2936.8870 (261)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 610.6628 ] }
    , { operator = Tj, parameters = [ Text "Water heating (other fuel)                                                                 2974.5429                0.2160              642.5013 (264)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 604.2559 ] }
    , { operator = Tj, parameters = [ Text "Space and water heating                                                                                                                3579.3882 (265)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 597.8489 ] }
    , { operator = Tj, parameters = [ Text "Pumps and fans                                                                              165.0000                0.5190               85.6350 (267)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 591.4419 ] }
    , { operator = Tj, parameters = [ Text "Energy for lighting                                                                         348.2015                0.5190              180.7166 (268)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 585.0349 ] }
    , { operator = Tj, parameters = [ Text "Total kg/year                                                                                                                          3845.7398 (272)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 578.6279 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 572.2209 ] }
    , { operator = Tj, parameters = [ Text "13a. Primary energy - Individual heating systems including micro-CHP" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 565.814 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 559.407 ] }
    , { operator = Tj, parameters = [ Text "                                                                                             Energy Primary energy factor        Primary energy" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfInt 553 ] }
    , { operator = Tj, parameters = [ Text "                                                                                           kWh/year            kg CO2/kWh              kWh/year" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 546.593 ] }
    , { operator = Tj, parameters = [ Text "Space heating - main system 1                                                             13596.6989                1.2200            16587.9726 (261)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 540.186 ] }
    , { operator = Tj, parameters = [ Text "Water heating (other fuel)                                                                 2974.5429                1.2200             3628.9424 (264)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 533.779 ] }
    , { operator = Tj, parameters = [ Text "Space and water heating                                                                                                               20216.9150 (265)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 527.3721 ] }
    , { operator = Tj, parameters = [ Text "Pumps and fans                                                                              165.0000                3.0700              506.5500 (267)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 520.9651 ] }
    , { operator = Tj, parameters = [ Text "Energy for lighting                                                                         348.2015                3.0700             1068.9786 (268)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 514.5581 ] }
    , { operator = Tj, parameters = [ Text "Primary energy kWh/year                                                                                                               21792.4436 (272)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 508.1511 ] }
    , { operator = Tj, parameters = [ Text "Primary energy kWh/m2/year                                                                                                              313.7409 (273)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 501.7441 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 495.3372 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 488.9302 ] }
    , { operator = Tj, parameters = [ Text "SAP 2012 EPC IMPROVEMENTS (Version 9.94, August 2019)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 482.5232 ] }
    , { operator = Tj, parameters = [ Text "-------------------------------------------------------------------------------------------------------------------------------------------------------------" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 476.1162 ] }
    , { operator = Tj, parameters = [ Text "Current energy efficiency rating:                                       D 60" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 469.7092 ] }
    , { operator = Tj, parameters = [ Text "Current environmental impact rating:                                    E 54" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 463.3022 ] }
    , { operator = Tj, parameters = [ Text "(For testing purposes):" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 456.8953 ] }
    , { operator = Tj, parameters = [ Text "A  Loft insulation                                                         Existing unknown" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 450.4883 ] }
    , { operator = Tj, parameters = [ Text "A2 Flat roof insulation                                                    Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 444.0813 ] }
    , { operator = Tj, parameters = [ Text "A3 Room-in-roof insulation                                                 Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 437.6743 ] }
    , { operator = Tj, parameters = [ Text "B  Cavity wall insulation                                                  Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 431.2673 ] }
    , { operator = Tj, parameters = [ Text "B4 Party wall insulation                                                   Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 424.8603 ] }
    , { operator = Tj, parameters = [ Text "Q  Internal or external wall insulation                                    Recommended" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 418.4534 ] }
    , { operator = Tj, parameters = [ Text "Q2 External and cavity wall insulation                                     Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 412.0464 ] }
    , { operator = Tj, parameters = [ Text "W1 Floor insulation (suspended floor)                                      Recommended" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 405.6394 ] }
    , { operator = Tj, parameters = [ Text "W2 Floor insulation (solid floor)                                          Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 399.2324 ] }
    , { operator = Tj, parameters = [ Text "C  Hot water cylinder insulation                                           Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 392.8254 ] }
    , { operator = Tj, parameters = [ Text "D  Draught proofing                                                        Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 386.4185 ] }
    , { operator = Tj, parameters = [ Text "E  Low energy lighting                                                     SAP increase too small" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 380.0115 ] }
    , { operator = Tj, parameters = [ Text "F  Cylinder thermostat                                                     Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 373.6045 ] }
    , { operator = Tj, parameters = [ Text "G  Heating controls                                                        Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 367.1975 ] }
    , { operator = Tj, parameters = [ Text "H  Heating controls                                                        Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 360.7905 ] }
    , { operator = Tj, parameters = [ Text "J  Biomass boiler                                                          Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 354.3835 ] }
    , { operator = Tj, parameters = [ Text "K  Biomass stove with boiler                                               Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 347.9766 ] }
    , { operator = Tj, parameters = [ Text "J2 Biomass boiler (alternative)                                            Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 341.5696 ] }
    , { operator = Tj, parameters = [ Text "Z1 Air or ground source heat pump (alternative)                            Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 335.1626 ] }
    , { operator = Tj, parameters = [ Text "Z2 Air or ground source heat pump with underfloor heating (alternative)    Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 328.7556 ] }
    , { operator = Tj, parameters = [ Text "Z3 Micro CHP (alternative)                                                 Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 322.3486 ] }
    , { operator = Tj, parameters = [ Text "I  Condensing boiler                                                       Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 315.9416 ] }
    , { operator = Tj, parameters = [ Text "R  Condensing oil boiler                                                   Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 309.5347 ] }
    , { operator = Tj, parameters = [ Text "S  Condensing boiler                                                       Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 303.1277 ] }
    , { operator = Tj, parameters = [ Text "T  Gas condensing boiler                                                   Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 296.7207 ] }
    , { operator = Tj, parameters = [ Text "T2 Flue gas heat recovery                                                  Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 290.3137 ] }
    , { operator = Tj, parameters = [ Text "L2 High heat retention storage heaters                                     Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 283.9067 ] }
    , { operator = Tj, parameters = [ Text "M  Replacement warm air unit                                               Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 277.4997 ] }
    , { operator = Tj, parameters = [ Text "N  Solar water heating                                                     Recommended" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 271.0928 ] }
    , { operator = Tj, parameters = [ Text "Y  Heat recovery system for mixer showers                                  Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 264.6858 ] }
    , { operator = Tj, parameters = [ Text "O  Double glazed windows                                                   Already installed" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 258.2788 ] }
    , { operator = Tj, parameters = [ Text "O3 Replacement glazing units                                               Unsuitable glazing" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 251.8718 ] }
    , { operator = Tj, parameters = [ Text "P  Secondary glazing                                                       Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 245.4648 ] }
    , { operator = Tj, parameters = [ Text "X  High performance external doors                                         SAP increase too small" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 239.0579 ] }
    , { operator = Tj, parameters = [ Text "U  Solar photovoltaic panels                                               Recommended" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 232.6509 ] }
    , { operator = Tj, parameters = [ Text "V2 Wind turbine                                                            Not applicable" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 226.2439 ] }
    , { operator = Tj, parameters = [ Text "Recommended measures:                     SAP change   Cost change     CO2 change" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 219.8369 ] }
    , { operator = Tj, parameters = [ Text "Q  Internal or external wall insulation    + 7.4        -£ 418          -883 kg (23.0%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 213.4299 ] }
    , { operator = Tj, parameters = [ Text "W1 Floor insulation (suspended floor)      + 1.7        -£  95          -200 kg (6.8%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 207.0229 ] }
    , { operator = Tj, parameters = [ Text "N  Solar water heating                     + 1.9        -£ 110          -245 kg (8.9%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 200.616 ] }
    , { operator = Tj, parameters = [ Text "U  Solar photovoltaic panels               + 11.7       -£ 675          -954 kg (37.9%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 194.209 ] }
    , { operator = Tj, parameters = [ Text "Measures omitted - SAP change or cost saving too small:" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 187.802 ] }
    , { operator = Tj, parameters = [ Text "E  Low energy lighting                     + 0.2        -£  11          -15 kg (0.5%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 181.395 ] }
    , { operator = Tj, parameters = [ Text "X  High performance external doors         + 0.5        -£  26          -56 kg (2.2%)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 174.988 ] }
    , { operator = Tj, parameters = [ Text "                                                                      Energy  Environmental" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 168.581 ] }
    , { operator = Tj, parameters = [ Text "                                         Typical annual savings       efficiency  impact" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 162.1741 ] }
    , { operator = Tj, parameters = [ Text "Recommended measures" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 155.7671 ] }
    , { operator = Tj, parameters = [ Text "Internal or external wall insulation      £418            12.72 kg/m" ] }
    , { operator = Ts, parameters = [ PdfFloat 1.1017 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 4.125 ] }
    , { operator = Tj, parameters = [ Text "2" ] }
    , { operator = Ts, parameters = [ PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Tj, parameters = [ Text "  D 67     D 64     green" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 149.3601 ] }
    , { operator = Tj, parameters = [ Text "Floor insulation (suspended floor)        £ 95             2.88 kg/m" ] }
    , { operator = Ts, parameters = [ PdfFloat 1.1017 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 4.125 ] }
    , { operator = Tj, parameters = [ Text "2" ] }
    , { operator = Ts, parameters = [ PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Tj, parameters = [ Text "  C 69     D 67     green" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 142.9531 ] }
    , { operator = Tj, parameters = [ Text "Solar water heating                       £110             3.52 kg/m" ] }
    , { operator = Ts, parameters = [ PdfFloat 1.1017 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 4.125 ] }
    , { operator = Tj, parameters = [ Text "2" ] }
    , { operator = Ts, parameters = [ PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Tj, parameters = [ Text "  C 71     C 70     orange" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 136.5461 ] }
    , { operator = Tj, parameters = [ Text "Solar photovoltaic panels                 £675            13.73 kg/m" ] }
    , { operator = Ts, parameters = [ PdfFloat 1.1017 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 4.125 ] }
    , { operator = Tj, parameters = [ Text "2" ] }
    , { operator = Ts, parameters = [ PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Tj, parameters = [ Text "  B 83     C 80     green" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 130.1392 ] }
    , { operator = Tj, parameters = [ Text "                          Total Savings  £1298           32.85 kg/m" ] }
    , { operator = Ts, parameters = [ PdfFloat 1.1017 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 4.125 ] }
    , { operator = Tj, parameters = [ Text "2" ] }
    , { operator = Ts, parameters = [ PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 123.7322 ] }
    , { operator = Tj, parameters = [ Text "Potential energy efficiency rating:                                    B 83" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 117.3252 ] }
    , { operator = Tj, parameters = [ Text "Potential environmental impact rating:                                          C 80" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 110.9182 ] }
    , { operator = Tj, parameters = [ Text "Fuel prices for cost data on this page from database revision number 527 TEST (27 Sep 2023)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 104.5112 ] }
    , { operator = Tj, parameters = [ Text "Recommendation texts and addenda revision number 6.1 (11 Jun 2019)" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 98.1042 ] }
    , { operator = Tj, parameters = [ Text "Typical heating and lighting costs of this home (per year, Thames Valley):" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 91.6973 ] }
    , { operator = Tj, parameters = [ Text "                                       Current        Potential      Saving" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 85.2903 ] }
    , { operator = Tj, parameters = [ Text " Electricity                          £188         £207         -£18" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 78.8833 ] }
    , { operator = Tj, parameters = [ Text " Mains gas                            £1798        £1157        £641" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 72.4763 ] }
    , { operator = Tj, parameters = [ Text " Space heating                        £1555        £1047        £508" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 66.0693 ] }
    , { operator = Tj, parameters = [ Text " Water heating                        £304         £189         £115" ] }
    , { operator = ET, parameters = [] }
    , { operator = BT, parameters = [] }
    , { operator = Tr, parameters = [ PdfInt 0 ] }
    , { operator = RgLowercase, parameters = [ PdfInt 0, PdfInt 0, PdfInt 0 ] }
    , { operator = Tf, parameters = [ Name "F1", PdfFloat 5.5 ] }
    , { operator = Td, parameters = [ PdfInt 30, PdfFloat 59.6623 ] }
    , { operator = Tj, parameters = [ Text " Lighting                             £128         £128         £0" ] }
    , { operator = ET, parameters = [] }
    ]
