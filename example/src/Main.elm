module Main exposing (main)

import BoundingBox2d
import Browser
import Bytes exposing (Bytes)
import Bytes.Encode
import Dict exposing (Dict)
import File.Download
import Hex.Convert
import Html exposing (Html)
import Html.Events
import Http exposing (Error(..), Response(..))
import Length exposing (Length, Meters)
import Pdf exposing (ASizes(..), Orientation(..), PageCoordinates, Pdf)
import Pixels
import Point2d exposing (Point2d)
import Vector2d


type Msg
    = Download
    | LoadImage String (Result Http.Error Bytes)


imageMissing : Bytes
imageMissing =
    "FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48 00 48 00 00 FF FE 00 13 43 72 65 61 74 65 64 20 77 69 74 68 20 47 49 4D 50 FF DB 00 43 00 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 FF DB 00 43 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 FF C2 00 11 08 00 28 00 28 03 01 11 00 02 11 01 03 11 01 FF C4 00 15 00 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 09 08 FF C4 00 17 01 00 03 01 00 00 00 00 00 00 00 00 00 00 00 00 00 07 08 09 06 FF DA 00 0C 03 01 00 02 10 03 10 00 00 01 8D 15 4B F0 D0 99 26 BA 4C 47 4B CE 21 D3 94 2A 86 69 73 42 64 9A E2 F0 6E 94 5E DB B5 46 09 C2 35 CC 29 7A 75 A8 44 D4 6C 2E 07 D4 A8 13 06 D9 5C DB 75 6D E6 3A CA 73 88 74 E5 0A A1 9A 5D 70 ED 95 F8 7B 12 D0 34 26 49 AE 2F 06 E9 43 0A 5E 9D 6A 11 35 1B 36 47 0E 80 BC 1B A5 1F FF C4 00 1E 10 00 02 02 02 02 03 00 00 00 00 00 00 00 00 00 00 02 07 01 03 05 06 00 22 08 10 15 FF DA 00 08 01 01 00 01 05 02 EF 51 A6 DC 91 9E 8E 38 DC 63 AF 09 11 DA 6E 44 DC 67 A3 BD 46 3E 40 6D 31 A6 91 1D A6 9A 4D 7C EF 5E 40 0E 9B 1B 4F 10 B4 E9 D7 6D BC 71 B8 C7 5E 12 23 B4 E9 42 ED B7 69 DD EA 34 DB 92 33 D0 44 76 9A 69 35 F3 BD 39 13 71 9E 8E F5 1F FF C4 00 2D 11 00 02 01 04 00 03 05 09 01 01 00 00 00 00 00 00 01 02 03 04 05 06 11 00 08 12 07 13 21 22 23 14 15 25 35 41 43 61 62 81 24 32 FF DA 00 08 01 03 01 01 3F 01 F4 A7 8B ED CD 0C D1 FE B2 45 2C 52 2F F5 1E 37 43 F9 56 53 F5 07 8E 63 B9 71 6C 55 AB 73 CC 0E 89 9F 18 76 7A 9B ED 8A 99 0B 36 38 CC 7A A4 AF A0 8D 76 5A C2 C4 96 9E 05 1B B3 9F 32 FC 2F E5 DC 72 E5 CB 93 E5 EF 47 9C E7 34 6F 1E 29 1B AC F6 6B 34 EA 51 F2 47 43 B4 AA AA 43 A6 5B 12 B0 DA A9 D1 BA 91 A1 FE 0D 9A B4 48 A0 89 51 15 21 86 14 08 88 81 63 8A 28 A3 5D 2A AA 8D 2A 22 28 00 00 02 AA 8D 0D 01 C7 2E 3C C7 36 2A D4 58 1E 79 5A CF 8C 3B 25 35 8A FB 52 E5 9B 1C 66 3D 31 D0 57 C8 DB 2D 61 62 42 C1 3B 1D D9 CF 95 BE 17 F2 EF 4A 78 BE DC D0 CD 1F EB 24 52 C5 22 FF 00 51 E3 74 3F 95 65 3F 50 78 7E 52 30 56 ED 1C 65 61 FA 71 23 BA F7 C2 44 3A A3 F7 C7 7B D5 DD AC E1 FC B8 FB 78 CE D6 AE 8F 2C A3 D9 52 51 6D 61 49 1A 24 50 44 A8 8A 90 C3 0A 04 44 40 B1 C5 14 51 AE 95 55 46 95 11 14 00 00 01 55 46 86 80 E3 98 FE 63 CD E0 D7 76 7F D9 FD 71 16 80 64 A5 C8 B2 2A 59 34 6E A4 6D 26 B5 DA E6 43 E1 6C 1E 29 59 58 87 E2 3E 30 40 7D 83 AD EB 78 E5 25 FB 47 6C 15 C6 56 37 89 2F 72 30 97 AF 32 FB E3 D8 C7 58 9D 63 EA FF 00 AC 7D 7C 9E EA 69 C8 95 7D 54 A5 EA B6 8A 41 1F 1C D6 D4 F6 8D 4D D9 FB 1C 35 35 8F 3F 7E B9 AD 45 0B 4B EF A8 2D 85 50 47 DD AA 0F 2D 99 FD 51 79 9A 16 F6 84 8B BA 12 85 B6 B5 7B 8E 39 72 E5 C9 F2 F7 A3 CE 73 9A 37 8F 14 8D D6 7B 35 9A 75 28 F9 23 A1 DA 55 55 21 D3 2D 89 58 6D 54 E8 DD 48 D0 FF 00 06 CD 5A 24 50 44 A8 8A 90 C3 0A 04 44 40 B1 C5 14 51 AE 95 55 46 95 11 14 00 00 01 55 46 86 80 E2 A7 9A DE CF E9 BB 46 4C 34 B7 5E 3C 3A A8 6A 33 55 9D 4D B2 0B D7 7A 11 63 EE C2 79 EC CB E6 86 6B C8 97 BA 4A 82 25 11 35 B5 5E BC 7A 53 C5 F6 E6 86 68 FF 00 59 22 96 29 17 FA 8F 1B A1 FC AB 29 FA 83 C7 31 DC B8 B6 2A D5 B9 E6 07 44 CF 8C 3B 3D 4D F6 C5 4C 85 9B 1C 66 3D 52 57 D0 46 BB 2D 61 62 4B 4F 02 8D D9 CF 99 7E 17 F2 E4 48 A0 89 51 15 21 86 14 08 88 81 63 8A 28 A3 5D 2A AA 8D 2A 22 28 00 00 02 AA 8D 0D 01 C7 31 FC C7 9B C1 AE EC FF 00 B3 FA E2 2D 00 C9 4B 91 64 54 B2 68 DD 48 DA 4D 6B B5 CC 87 C2 D8 3C 52 B2 B1 0F C4 7C 60 80 FB 07 5B D6 F1 CB 8F 31 CD 8A B5 16 07 9E 56 B3 E3 0E C9 4D 62 BE D4 B9 66 C7 19 8F 4C 74 15 F2 36 CB 58 58 90 B0 4E C7 76 73 E5 6F 85 FC BB D2 9E 2F B7 34 33 47 FA C9 14 B1 48 BF D4 78 DD 0F E5 59 4F D4 1E 3F FF C4 00 33 11 00 01 01 04 06 07 08 02 03 00 00 00 00 00 00 00 01 02 03 04 05 11 00 06 23 31 33 41 12 13 14 21 22 44 91 15 24 32 43 52 54 61 81 34 73 10 25 42 FF DA 00 08 01 02 01 01 3F 01 E2 4A B3 4A 92 7E 42 92 A0 7A 82 0F D8 34 A9 F5 C0 3F 06 70 B8 A3 49 3E 89 21 D5 E9 66 41 F0 0B 99 35 3E EB D2 AE 62 E3 6D 8B 4A DF 5B C4 3C 34 86 43 1A 02 FC 41 4B C3 C2 4C C3 98 37 A1 07 37 A3 99 F2 3F 6E 19 25 44 92 4A 94 A3 32 4E F2 A2 73 39 92 4F 5A 57 0A 9E 1F 83 48 A4 2D 9C 9F 44 D6 F4 EA 81 20 F8 05 ED 59 0F 75 EA 4F 31 78 B6 C5 E2 4A B3 4A 92 7E 42 92 A0 7A 82 0F D8 34 15 FA 29 D8 FB 0C BB FE 10 89 69 5A 6C F2 94 CA 65 F9 7F E7 5F 3B B8 CA 75 D6 84 92 A2 49 25 4A 51 99 27 79 51 39 9C C9 27 AD 2A 7D 4F D9 F5 51 58 AB 2E F1 B9 6E 8E 8B 18 19 A5 B3 64 9F 3B 36 6C CE 17 89 56 B2 0C E9 5F 84 1F B5 06 C3 F9 FC 5D A4 19 4B 67 D6 6E D1 26 5C DD FA FD 1D DE 12 BB 6D 64 E9 51 11 07 5C 57 FB 03 DE C6 89 86 A1 AC B6 75 36 DF 39 93 7B C0 E1 D9 D2 AE 12 74 B4 6D B5 43 F8 AD F5 BC 43 C3 48 64 31 A0 2F C4 14 BC 3C 24 CC 39 83 7A 10 73 7A 39 9F 23 F6 E1 92 54 49 24 A9 4A 33 24 EF 2A 27 33 99 24 F5 A2 2A 24 55 70 73 10 B9 EF 15 10 D2 9B 65 3B CA 64 CE 7B 9E 0F 89 2E FA 33 29 E1 D2 D7 49 95 38 92 AC D2 A4 9F 90 A4 A8 1E A0 83 F6 0D 2A 7D 70 0F C1 9C 2E 28 D2 4F A2 48 75 7A 59 90 7C 02 E6 4D 4F BA F4 AB 98 B8 DB 62 92 54 49 24 A9 4A 33 24 EF 2A 27 33 99 24 F5 A5 4F A9 FB 3E AA 2B 15 65 DE 37 2D D1 D1 63 03 34 B6 6C 93 E7 66 CD 99 C2 F1 2A D6 41 9D 2B 85 4F 0F C1 A4 52 16 CE 4F A2 6B 7A 75 40 90 7C 02 F6 AC 87 BA F5 27 98 BC 5B 62 F1 25 59 A5 49 3F 21 49 50 3D 41 07 EC 1A 7F FF C4 00 2E 10 00 02 01 02 04 03 08 02 02 03 00 00 00 00 00 00 01 02 03 04 05 06 11 12 21 00 14 31 13 15 22 24 34 35 42 61 23 81 44 64 25 33 36 FF DA 00 08 01 01 00 06 3F 02 F9 47 24 6D F6 8E 8E 87 F4 CA CA C3 E8 82 38 A6 C2 B8 AA A4 2D ED 42 C3 6B BA 4C D9 0B C0 1B 25 2D 53 9D 85 D4 0D A3 90 FB 8F 43 E7 BD 67 15 18 5F 0B D4 2B DF 9D 4C 57 1B 94 44 32 D9 95 86 4D 04 0C 36 37 42 3A 9E 94 03 FB 59 08 0B 31 69 24 91 8B 33 31 2C EE EC 73 24 93 99 66 62 73 24 EE 4F 15 38 AB 0A D3 05 BD A8 69 AE 96 B8 57 21 78 03 77 AA A5 41 B0 BA 81 BC 91 8F 71 EA 3C F7 AC F9 47 24 6D F6 8E 8E 87 F4 CA CA C3 E8 82 38 EE 1D 39 E2 0F 4A B8 98 C9 E6 3B BB 46 5A DA 2D 3B DD 87 FA C5 7E AD D3 F3 B4 7C E0 E6 1C B3 16 92 49 18 B3 33 12 CE EE C7 32 49 39 96 66 27 32 4E E4 F1 4B 8B 71 6D 2E 77 03 A6 7B 3D 9E 75 F4 1F 28 EB AB A3 6F E6 F4 6A 7A 76 1E 53 69 65 1C D6 95 A6 E1 7B 87 FE 80 F6 87 13 2D 2E 8E EE E6 3C 3D 93 3E 5D 2E C7 C5 CF 88 FC 07 F1 B4 F9 56 73 1A F8 03 11 B7 F9 75 EC 8E 1A 86 A8 27 76 C9 5B 9B 6B D4 58 EF 72 5F 01 B7 47 20 EC 99 FB 4D 04 D6 0A 55 3C 54 61 7C 2F 50 AF 7E 75 31 5C 6E 51 10 CB 66 56 19 34 10 30 D8 DD 08 EA 7A 50 0F ED 64 20 2C C5 A4 92 46 2C CC C4 B3 BB B1 CC 92 4E 65 99 89 CC 93 B9 3C 36 23 03 4D DF 6A A8 70 D1 88 F3 B2 5B 74 16 2D AF 57 86 E4 76 92 3B 71 8F 5B 45 9A 76 82 B0 AD 29 F9 47 24 6D F6 8E 8E 87 F4 CA CA C3 E8 82 38 A6 C2 B8 AA A4 2D ED 42 C3 6B BA 4C D9 0B C0 1B 25 2D 53 9D 85 D4 0D A3 90 FB 8F 43 E7 BD 61 66 2D 24 92 31 66 66 25 9D DD 8E 64 92 73 2C CC 4E 64 9D C9 E2 97 16 E2 DA 5C EE 07 4C F6 7B 3C EB E8 3E 51 D7 57 46 DF CD E8 D4 F4 EC 3C A6 D2 CA 39 AD 2B 4D C5 4E 2A C2 B4 C1 6F 6A 1A 6B A5 AE 15 C8 5E 00 DD EA A9 50 6C 2E A0 6F 24 63 DC 7A 8F 3D EB 3E 51 C9 1B 7D A3 A3 A1 FD 32 B2 B0 FA 20 8E 3F FF C4 00 1C 10 01 00 02 02 03 01 00 00 00 00 00 00 00 00 00 00 01 00 11 10 21 41 E1 F0 31 FF DA 00 08 01 01 00 01 3F 21 F5 FD DE 8E 39 20 56 A9 87 C6 10 00 F8 00 7D 1C 36 56 05 27 00 7F 47 68 B6 DC 29 7E A0 7F 5E 5F 20 BD 55 58 15 AA 61 F4 84 00 9E 00 4F 62 3D 7F 77 A3 8E 48 A9 52 AB A6 CB 47 0E CD FB 0F 04 BF 50 3F AF 2F 90 5E AA AC 00 00 2F 30 34 01 4A 30 F4 1D 91 55 25 DC B2 1F 3E 17 EE 35 85 BA 34 31 6D C3 21 7D F6 2D B6 56 05 27 00 7F 47 68 B6 DC 29 7E A0 7F 5E 5F 20 BD 55 59 73 12 86 EF 4C 38 3E 75 87 BD 7F 77 A3 8E 48 15 AA 61 F1 84 00 3E 00 1F 46 12 FD 40 FE BC BE 41 7A AA B0 00 00 BC C0 D0 05 28 C3 D0 70 AD 53 0F A4 20 04 F0 02 7B 11 EB FB BD 1C 72 4F FF DA 00 0C 03 01 00 02 00 03 00 00 00 10 CC 32 F1 24 11 C3 3F 32 43 DF FF C4 00 1A 11 01 00 03 01 01 01 00 00 00 00 00 00 00 00 00 00 01 00 10 F0 21 11 31 FF DA 00 08 01 03 01 01 3F 10 C9 CA 35 5A 85 3B 10 C2 82 4A 20 14 9A D2 A1 59 5F DD 2D 0A 0D C7 02 60 1F 18 40 16 9F DE 20 00 03 B1 0C 28 A4 A2 01 43 AC 67 27 28 D5 6A 1F A6 00 30 01 39 D0 09 59 0A 40 3E 30 80 2D 3F BC 40 00 38 05 24 80 1C 50 1F A8 A9 7E 4C 03 05 E0 72 F0 16 00 DA 51 E0 21 F1 E1 4A AE 5F A9 48 E1 59 5F DD 2D 0A 0D C7 02 60 1F 18 40 16 9F DE 20 00 1C 82 13 E9 EA A2 B2 3E 92 3E 4E 51 AA D4 29 D8 86 14 12 51 00 A4 D6 92 03 E3 08 02 D3 FB C4 00 03 80 52 48 01 C5 01 FA 8A 97 0E C4 30 A2 92 88 05 0E B1 9C 9C A3 55 A8 7F FF C4 00 1D 11 01 01 00 02 02 03 01 00 00 00 00 00 00 00 00 00 01 11 00 41 31 61 10 21 F0 71 FF DA 00 08 01 02 01 01 3F 10 F9 7D 16 87 74 0D 26 02 F1 00 00 8C C0 00 08 81 3C 19 C6 73 C1 A0 D4 03 02 85 AF 03 09 C2 28 2E EA 8D 48 AA AA 9A D5 C0 5E 20 00 14 98 00 14 00 26 1F CB E8 B4 3B A0 69 30 09 D1 F6 6B F4 A8 71 E7 76 07 04 E1 14 17 75 46 A4 55 55 4D 6A E1 91 94 5F C2 C0 C8 BF 40 6A 7E 04 08 E3 AE 64 E9 4B 8E DF 05 2F 00 25 A5 09 EA B1 B4 21 69 21 30 E3 39 E0 D0 6A 01 81 42 D7 81 84 E1 14 17 75 46 A4 55 55 4D 6A E0 44 AC 42 5F A2 38 49 8E A0 93 E5 F4 5A 1D D0 34 98 0B C4 00 02 33 00 00 22 04 C3 4E 11 41 77 54 6A 45 55 54 D6 AE 19 19 45 FC 2C 0C 8B F4 06 A7 E0 41 78 80 00 52 60 00 50 00 98 7F 2F A2 D0 EE 81 A4 CF FF C4 00 1A 10 01 00 03 01 01 01 00 00 00 00 00 00 00 00 00 00 01 00 10 F0 21 11 31 FF DA 00 08 01 01 00 01 3F 10 CA A2 30 82 20 28 39 2B A3 05 0C 73 1A 2F A5 DD AE 4C 6A AD 23 8C 82 54 62 21 84 40 44 12 85 07 65 74 60 A1 8E 63 0E 55 11 84 11 1F 35 C5 9D 25 4E 05 C0 33 68 82 54 62 21 84 40 44 12 97 02 42 6D BD 48 80 15 97 43 BA 79 20 18 55 D0 1D 11 D3 46 04 C5 2F 7C 30 25 28 20 31 F4 BB B5 C9 8D 55 A4 71 90 4A 8C 44 30 88 08 82 53 10 32 57 F8 30 2B 04 40 8B 2A 88 C2 08 80 A0 E4 AE 8C 14 31 CC 61 41 2A 31 10 C2 20 22 09 4B 81 21 36 DE A4 40 0A CB A0 14 1D 95 D1 82 86 39 8C 39 54 46 10 44 7F FF D9"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (Bytes.Encode.sequence [] |> Bytes.Encode.encode)


titleFontSize =
    Length.points 100


normalFontSize =
    Length.points 50


slide : List Pdf.Item -> Pdf.Page
slide contents =
    Pdf.page
        { size = Vector2d.xy (Length.points 1920) (Length.points 1080)
        , contents = contents
        }


position : Float -> Float -> Point2d Meters PageCoordinates
position x y =
    Point2d.xy (Length.points x) (Length.points y)


defaultFont =
    Pdf.helvetica { bold = False, oblique = False }


margin =
    100


pdf : Loaded_ -> Bytes
pdf images =
    Pdf.pdf
        { title = "PDF presentation"
        , pages =
            [ slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin 400)
                    "PDFs in Elm"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 550)
                    "By Martin Stewart"
                , Pdf.imageFit
                    (BoundingBox2d.withDimensions ( Length.points 259, Length.points 257 ) (position 1000 500))
                    images.pdfLogo
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "What's a PDF anyway?"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    """It's a weird mix of ascii text and binary data

Long ago it was an Adobe proprietary format but now it's an ISO standard

It's really complicated and filled with legacy cruft

It's (unfortunately) also a really popular format
"""
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "Lets add support for it using pure Elm!"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    "How hard can it be?"
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "Answer"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    "It depends."
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "Answer continued..."
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    """More specifically it depends on what you want to achieve.

Create a standards compliant PDF parser?
        Extremely time consuming, difficult, and unusably slow

Create a fully featured PDF encoder?
        Extremely time consuming and difficult

Create a PDF encoder for a small subset of the standard?
        Doable!
"""
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position 800 margin)
                    "But why?"
                , Pdf.imageFit
                    (BoundingBox2d.withDimensions ( Length.points 1280, Length.points 720 ) (position 960 600))
                    images.butWhy
                ]
            , slide
                [ Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    """At work we deal with generating PDFs sometimes

It's one fewer Javascript dependencies

If it gains traction and others contribute PRs, it might one day become
a fully fledged PDF package with my name on it!
        Aka delusions of grandeur"""
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "So what can it do?"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    "Ascii text!"
                , Pdf.text
                    normalFontSize
                    (Pdf.timesRoman { bold = True, italic = True })
                    (position margin 370)
                    "Multiple fonts!"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 440)
                    "Jpeg images!"
                , Pdf.imageFit
                    (BoundingBox2d.withDimensions ( Length.points 300, Length.points 300 ) (position (margin + 150) 670))
                    images.jpeg
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin (margin - 20))
                    "What can't it do?"
                , Pdf.imageFit
                    (BoundingBox2d.withDimensions ( Length.points 200, Length.points 95 ) (position (margin + 955) 232))
                    images.unicode
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 200)
                    """Unicode characters: 你好 👋   vs

Automatically line breaking text that is too long to fit within the width of a single page
        Or really any features for easily laying out text

Fonts that aren't Courier, Helvetica, Times Roman, or Wingdings

ONLY jpeg images"""
                , Pdf.imageFit
                    (BoundingBox2d.withDimensions ( Length.points 300, Length.points 300 ) (position (margin + 150) 780))
                    images.deepfriedJpeg
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 950)
                    "All that other stuff like text effects, buttons, links, videos, etc."
                ]
            , slide
                [ Pdf.text
                    titleFontSize
                    defaultFont
                    (position margin margin)
                    "That's all folks!"
                , Pdf.text
                    normalFontSize
                    defaultFont
                    (position margin 300)
                    "As you probably guessed, this presentation was generated with Elm code."
                ]
            ]
        }
        |> Pdf.toBytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Download, Loaded pdfBytes ) ->
            ( model, File.Download.bytes "Example pdf.pdf" "application/pdf" pdfBytes )

        ( LoadImage imageName result, Loading loading ) ->
            let
                newLoading =
                    Dict.insert imageName result loading

                getImage_ name =
                    case Dict.get name newLoading of
                        Just httpResult ->
                            case httpResult of
                                Ok bytes ->
                                    Pdf.jpeg name bytes

                                Err _ ->
                                    Nothing

                        Nothing ->
                            Nothing
            in
            ( Maybe.map5
                Loaded_
                (getImage_ "pdfLogo")
                (getImage_ "butWhy")
                (getImage_ "jpeg")
                (getImage_ "deepfriedJpeg")
                (getImage_ "unicode")
                |> Maybe.map (pdf >> Loaded)
                |> Maybe.withDefault (Loading newLoading)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading dict ->
            "Loading... "
                ++ String.fromInt (Dict.size dict)
                ++ "/"
                ++ String.fromInt (List.length imagesToLoad)
                |> Html.text

        Loaded _ ->
            Html.button [ Html.Events.onClick Download ] [ Html.text "Download" ]


getImage : String -> String -> Cmd Msg
getImage imageName url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "origin" "null" ]
        , url = url
        , body = Http.emptyBody
        , expect =
            Http.expectBytesResponse
                (LoadImage imageName)
                (\response ->
                    case response of
                        BadUrl_ url_ ->
                            BadUrl url_ |> Err

                        Timeout_ ->
                            Err Timeout

                        NetworkError_ ->
                            Err NetworkError

                        BadStatus_ metadata _ ->
                            BadStatus metadata.statusCode |> Err

                        GoodStatus_ _ body ->
                            Ok body
                )
        , timeout = Nothing
        , tracker = Nothing
        }


type Model
    = Loading (Dict String (Result Http.Error Bytes))
    | Loaded Bytes


type alias Loaded_ =
    { pdfLogo : Pdf.Image
    , butWhy : Pdf.Image
    , jpeg : Pdf.Image
    , deepfriedJpeg : Pdf.Image
    , unicode : Pdf.Image
    }


imagesToLoad =
    [ getImage "pdfLogo" "https://cors-anywhere.herokuapp.com/https://fmfencing.com/images/stories/PDF-Icon.jpg"
    , getImage "butWhy" "https://cors-anywhere.herokuapp.com/https://i.ytimg.com/vi/3Z9yK3sMDUU/maxresdefault.jpg"
    , getImage "jpeg" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716251113011019776/jpeg.jpg"
    , getImage "deepfriedJpeg" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716251175271268352/deepfried_jpeg.jpg"
    , getImage "unicode" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716292238086242344/wave.jpg"
    ]


init _ =
    ( Loading Dict.empty
    , Cmd.batch imagesToLoad
    )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
