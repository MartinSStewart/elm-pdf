module Main exposing (..)

import BoundingBox2d
import Browser
import Bytes exposing (Bytes)
import Bytes.Encode
import Dict
import File.Download
import Hex.Convert
import Html
import Html.Events
import Length
import Pdf exposing (ASizes(..), Orientation(..))
import Pixels
import Point2d


type alias Model =
    ()


type Msg
    = Download


imageData : Bytes
imageData =
    "FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48 00 48 00 00 FF FE 00 13 43 72 65 61 74 65 64 20 77 69 74 68 20 47 49 4D 50 FF DB 00 43 00 03 02 02 03 02 02 03 03 03 03 04 03 03 04 05 08 05 05 04 04 05 0A 07 07 06 08 0C 0A 0C 0C 0B 0A 0B 0B 0D 0E 12 10 0D 0E 11 0E 0B 0B 10 16 10 11 13 14 15 15 15 0C 0F 17 18 16 14 18 12 14 15 14 FF DB 00 43 01 03 04 04 05 04 05 09 05 05 09 14 0D 0B 0D 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 FF C2 00 11 08 00 C8 00 64 03 01 11 00 02 11 01 03 11 01 FF C4 00 1A 00 01 01 01 01 01 01 01 00 00 00 00 00 00 00 00 00 00 08 07 05 04 06 03 FF C4 00 1B 01 01 00 03 01 01 01 01 00 00 00 00 00 00 00 00 00 00 05 06 07 04 08 03 02 FF DA 00 0C 03 01 00 02 10 03 10 00 00 01 FC 6A BB E6 7F 1F 6D 00 00 00 1E 8F D7 CA 8A B0 E3 B3 6D 73 67 00 00 00 00 00 35 E9 7C F7 EE 3B 6A E0 00 04 F9 01 AE F3 3E 5D A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 A5 AC 98 AE B3 2D 40 00 00 22 EA 6F A5 38 3C F2 A0 0A 5A C9 8A EB 32 D4 00 00 02 2E A6 FA 53 83 CF 2A 00 D7 A5 F3 DF B8 ED AB 80 00 13 E4 06 BB CC F9 76 80 00 00 00 00 1A 04 85 4B 3F 8F B6 80 00 00 0F 47 EB E5 45 58 71 DF FF C4 00 1E 10 00 02 01 05 01 01 01 00 00 00 00 00 00 00 00 00 05 20 35 00 03 04 07 17 06 30 02 FF DA 00 08 01 01 00 01 05 02 F5 3E A6 F9 BC BF 95 8B F7 71 6E 82 F7 D8 97 47 FD FC 8F 91 C3 3E 37 9A 0C AE 68 32 B9 A0 CA E6 83 2B 9A 0C AE 68 32 B9 A0 CA 27 8D F9 C2 24 9A D2 09 CF CE A6 B4 82 73 F3 A9 AD 20 9C FC EA 6B 48 27 3F 3A 9A D2 09 CF CE A6 B4 82 73 F3 A9 AD 20 9C FC EA 6B 48 27 3F 3A 9A D2 09 CF CE A6 B4 82 73 F3 A9 AD 20 9C FC EA 6B 48 27 3F 3A 9A D2 09 CF CE A6 B4 82 73 F3 A9 AD 20 9C FC EA 6B 48 27 3F 3A 9A D2 09 CF CE A6 B4 82 73 F3 A9 AD 20 9C FC EA 79 1F 5D 86 00 6F 4B 19 5D 2C 65 74 B1 95 D2 C6 57 4B 19 5D 2C 65 74 B1 94 4F 27 F3 9A 4B EF EA 7C B5 F0 99 7F 2B 16 2E E5 5D 05 E0 71 2D 0F FF C4 00 2D 11 00 00 02 07 07 04 01 05 00 00 00 00 00 00 00 00 01 02 03 04 05 06 35 72 B1 00 16 20 30 52 91 D2 11 21 31 61 51 12 15 40 41 71 FF DA 00 08 01 03 01 01 3F 01 61 30 90 B3 10 95 22 42 F5 4A 3E 47 E3 D0 53 B7 9F E7 40 0C A4 A8 91 A7 20 A3 4A 50 30 0F E8 7B D9 AA E9 AC 91 64 7E DE 4F A9 18 FB 0E DE BB 8F 7F C0 78 5E 15 A6 4A D1 50 20 29 44 04 BD 7B F5 F9 1F 61 F1 6B EA D0 D0 4D 87 95 AF AB 43 41 36 1E 56 BE AD 0D 04 D8 79 5A FA B4 34 13 61 E5 6B EA D0 D0 4D 87 95 AF AB 43 41 36 1E 56 BE AD 0D 04 D8 79 59 45 31 96 55 51 27 3F 93 14 07 70 C2 FA C4 09 20 54 D9 0C 98 7A BC 85 A0 61 7D 62 04 90 2A 6C 86 4C 3D 5E 42 D0 30 BE B1 02 48 15 36 43 26 1E AF 21 68 18 5F 58 81 24 0A 9B 21 93 0F 57 90 B4 0C 2F AC 40 92 05 4D 90 C9 87 AB C8 5A 06 17 D6 20 49 02 A6 C8 64 C3 D5 E4 2D 03 0B EB 10 24 81 53 64 32 61 EA F2 16 81 85 F5 88 12 40 A9 B2 19 30 F5 79 0B 40 C2 FA C4 09 20 54 D9 0C 98 7A BC 85 A0 61 7D 62 04 90 2A 6C 86 4C 3D 5E 42 D0 30 BE B1 02 48 15 36 43 26 1E AF 21 68 18 5F 58 81 24 0A 9B 21 93 0F 57 90 B4 0C 2F AC 40 92 05 4D 90 C9 87 AB C8 5A 06 17 D6 20 49 02 A6 C8 64 C3 D5 E4 2D 03 0B EB 10 24 81 53 64 32 61 EA F2 16 81 85 F5 88 12 40 A9 B2 19 30 F5 79 0B 40 C2 FA C4 09 20 54 D9 0C 98 7A BC 85 A0 61 7D 62 04 90 2A 6C 86 4C 3D 5E 42 D0 30 BE B1 02 48 15 36 43 26 1E AF 21 68 18 5E 17 79 69 AC B4 54 E8 0C 50 00 2F 4E FD 7E 47 D0 FC DA E5 34 35 93 71 E3 6B 94 D0 D6 4D C7 8D AE 53 43 59 37 1E 36 B9 4D 0D 64 DC 78 DA E5 34 35 93 71 E3 6B 94 D0 D6 4D C7 8D AE 53 43 59 37 1E 36 51 42 65 65 54 48 0F E4 A5 00 D8 3F 01 84 DD 42 D3 42 54 69 0D D1 28 79 0F 9F 61 5E DE 3F 9D 04 72 92 A5 46 80 82 91 29 80 A0 1F B1 ED 66 AB D8 B2 75 91 FB 79 FE 94 61 E8 3B FB EE 1D AD FF C4 00 26 11 00 00 05 04 02 02 01 05 00 00 00 00 00 00 00 00 01 02 03 33 71 00 05 20 30 15 51 04 11 12 21 23 32 40 41 FF DA 00 08 01 02 01 01 3F 01 F2 BC A3 2C 6F 41 F8 EB 03 09 47 D9 69 0F 3C 82 4F BA 3F 5F D0 F1 3C 42 2E 4F 91 AB 8D 4B B1 AE 35 2E C6 B8 D4 BB 1A E3 52 EC 6B 8D 4B B1 AE 35 2E C6 B8 D4 BB 1A 50 BF 03 98 A1 FC C6 DA D0 CE 85 DD 3C 8E 36 D6 86 74 2E E9 E4 71 B6 B4 33 A1 77 4F 23 8D B5 A1 9D 0B BA 79 1C 6D AD 0C E8 5D D3 C8 E3 6D 68 67 42 EE 9E 47 1B 6B 43 3A 17 74 F2 38 DB 5A 19 D0 BB A7 91 C6 DA D0 CE 85 DD 3C 8E 36 D6 86 74 2E E9 E4 71 B6 B4 33 A1 77 4F 23 8D B5 A1 9D 0B BA 79 1C 6D AD 0C E8 5D D3 C8 E3 6D 68 67 42 EE 9E 47 1B 6B 43 3A 17 74 F2 38 DB 5A 19 D0 BB A7 91 C6 DA D0 CE 85 DD 3C 8E 36 D6 86 74 2E E9 E4 71 B6 B4 33 A1 77 4F 23 8F 89 E5 91 02 7C 4D 5C 92 5D 0D 72 49 74 35 C9 25 D0 D7 24 97 43 5C 92 5D 0D 72 49 74 35 C9 25 D0 D2 86 F9 9C C6 0F EF E8 79 5E 29 91 37 B0 FC 75 81 44 C3 E8 B4 87 80 40 27 DD 0F AD 7F FF C4 00 2A 10 00 01 02 03 08 01 03 05 00 00 00 00 00 00 00 00 01 02 03 04 74 B2 00 05 11 20 30 33 92 D2 12 21 31 61 15 32 40 52 F0 FF DA 00 08 01 01 00 06 3F 02 71 B6 DC 28 81 49 F1 42 12 4E 0B 18 FD C7 FB D3 4C 38 CB 8A 69 C1 EC B4 1C 08 B2 7E A6 F0 66 29 27 C4 E0 85 1F 31 FB 7A 0F 4F C0 72 22 21 C7 D0 B4 BA 51 83 64 61 86 03 E3 E6 DB F1 7C D3 D6 DB F1 7C D3 D6 DB F1 7C D3 D6 DB F1 7C D3 D6 DB F1 7C D3 D6 DB F1 7C D3 D6 DB F1 7C D3 D6 D1 70 E8 24 A1 A7 54 80 55 EF 80 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 F9 93 4A 74 2F 19 97 2A 39 5F 99 34 A7 42 F1 99 72 A3 95 C8 78 86 DF 5A D4 E9 5E 2D 81 86 18 0F 9F 8B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 6C 45 F0 4F 6B 45 C4 20 10 87 5D 52 C0 57 BE 04 FE 03 8E 36 D9 5C 0A 8F 92 16 90 70 40 C7 ED 3F DE BA 61 B6 5B 53 AE 1F 64 20 62 4D 93 F5 36 43 D1 4A 3E 47 05 A8 78 0F D7 D0 FA DB FF C4 00 1F 10 01 01 00 01 03 05 01 00 00 00 00 00 00 00 00 00 01 11 21 30 41 61 00 31 40 51 A1 70 FF DA 00 08 01 01 00 01 3F 21 71 F4 0C 16 03 8A B0 41 FA AB A5 51 BF 4E E2 31 38 EB 38 30 0C 81 32 04 FA E3 6B 3C 03 52 14 78 3E EB 39 68 22 44 89 12 24 48 BB D0 A7 C2 42 F3 8F DF 03 96 0E 58 39 60 E5 83 96 0E 58 39 60 E5 83 96 0E 58 39 60 E5 83 96 0E 58 39 60 E5 83 96 0E 58 39 63 52 15 7A 1E E3 39 68 22 44 89 12 24 48 BB 50 A7 C2 52 F3 9F 01 C7 D1 31 58 2E 62 50 17 EA 86 95 46 FD 39 8A C0 E3 AC 60 C0 32 04 C0 11 EF 9D E5 EB FF DA 00 0C 03 01 00 02 00 03 00 00 00 10 3F FF 00 FF 00 FF 00 A5 FF 00 FF 00 FF 00 FF 00 FF 00 F0 DB 6D B9 FF 00 EE 49 24 93 FF 00 DC 92 49 27 FF 00 B9 24 92 4F FF 00 72 49 24 9F FE E4 92 49 3F FD C9 24 92 7F FB 92 49 24 FF 00 F7 24 92 49 FF 00 EE 49 24 93 FF 00 DC 92 49 27 FF 00 B9 24 92 4F FF 00 72 49 24 9F FE E4 92 49 3F FD C9 24 92 7F FB 92 49 24 FF 00 F7 24 92 49 FF 00 EE 49 24 93 FF 00 DC 92 49 27 FF 00 B9 24 92 4F FF 00 0D B6 DB 9F FF 00 FF 00 FF 00 FF 00 FF 00 3F FF 00 FF 00 FF 00 A5 FF C4 00 1E 11 01 01 00 01 04 03 01 00 00 00 00 00 00 00 00 00 01 11 31 21 30 41 71 00 40 51 70 FF DA 00 08 01 03 01 01 3F 10 30 02 14 05 69 17 28 00 A9 5C D5 DB 0C A0 88 00 C6 94 68 C4 1E FC AE 40 A5 1B 56 D8 20 82 29 84 15 45 77 C3 80 1A 53 51 E0 A4 1C 7D D7 61 93 26 4C 99 32 64 24 02 B0 C5 05 95 59 5D 2A F7 FB E3 26 CC 9B 32 6C C9 B3 26 CC 9B 32 6C C9 B3 26 CC 9B 32 6C C9 B3 26 CC 9B 32 6C C9 B3 26 CC 9B 32 6C 1C 00 C6 1A 2F 05 20 E7 EE 9B 0C 99 32 64 C9 93 25 20 55 A6 28 0C A0 CA 69 43 AF 40 C0 08 11 05 85 4C 82 20 A0 73 13 6C 32 82 24 01 58 55 81 54 3B F2 B9 02 15 6C 5B 28 86 80 2E 01 41 50 FF C4 00 25 11 00 01 00 0A 03 00 03 01 00 00 00 00 00 00 00 00 01 00 20 21 30 51 A1 B1 D1 F0 F1 11 31 61 40 41 71 91 FF DA 00 08 01 02 01 01 3F 10 22 3F 01 D0 8F A6 AD EB F7 92 5D 0B 18 82 3E C3 0A 02 32 1F 0B 7D 60 67 C0 3F 39 04 1E 19 C4 07 86 29 B4 16 4D A0 B2 6D 05 93 68 2C 9B 41 64 DA 0B 26 D0 59 07 D2 88 8F E1 56 69 40 E3 31 12 AC D2 81 C6 62 25 59 A5 03 8C C4 4A B3 4A 07 19 88 95 66 94 0E 33 11 2A CD 28 1C 66 22 55 9A 50 38 CC 44 AB 34 A0 71 98 89 56 69 40 E3 31 12 AC D2 81 C6 62 25 59 A5 03 8C C4 4A B3 4A 07 19 88 95 66 94 0E 33 11 2A CD 28 1C 66 22 55 9A 50 38 CC 44 AB 34 A0 71 98 89 56 69 40 E3 31 12 AC D2 81 C6 62 25 59 A5 03 8C C4 4A A7 E3 24 93 CB 38 80 F4 41 34 82 E9 A4 17 4D 20 BA 69 05 D3 48 2E 9A 41 74 D2 0B A0 FA 51 13 FD 3F 00 88 7C 97 46 1E 1A 37 BF DE 40 74 2C 22 49 FA 0D 28 08 D8 FD 2C F1 85 A9 FF C4 00 1C 10 01 01 00 03 00 03 01 00 00 00 00 00 00 00 00 00 01 11 21 30 41 00 31 40 70 FF DA 00 08 01 01 00 01 3F 10 08 06 91 42 90 98 73 00 01 AC 3D E7 A5 20 51 09 52 31 C8 A7 7C 2A 77 02 52 21 70 22 19 5A 00 0D EF 58 DD A0 2A 3F 2E D2 06 34 19 32 64 C9 93 26 54 A3 38 28 B2 00 50 2C 02 F0 FD F1 A2 06 88 1A 20 68 81 A2 06 88 1A 20 68 81 A2 06 88 1A 20 68 81 A2 06 88 1A 20 68 81 A2 06 88 1A 20 3A C6 ED 01 15 F9 72 91 33 A0 C9 93 26 4C 99 32 25 19 C0 05 80 A0 01 62 97 AF C0 10 0D 22 81 25 31 E6 04 47 58 7B CF 4A 40 82 56 05 61 80 5E 78 D4 EE 44 A0 46 60 54 12 B0 40 5F FF D9"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (Bytes.Encode.sequence [] |> Bytes.Encode.encode)


image2Data : Bytes
image2Data =
    "FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48 00 48 00 00 FF FE 00 13 43 72 65 61 74 65 64 20 77 69 74 68 20 47 49 4D 50 FF DB 00 43 00 03 02 02 03 02 02 03 03 03 03 04 03 03 04 05 08 05 05 04 04 05 0A 07 07 06 08 0C 0A 0C 0C 0B 0A 0B 0B 0D 0E 12 10 0D 0E 11 0E 0B 0B 10 16 10 11 13 14 15 15 15 0C 0F 17 18 16 14 18 12 14 15 14 FF DB 00 43 01 03 04 04 05 04 05 09 05 05 09 14 0D 0B 0D 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 FF C2 00 11 08 00 46 00 67 03 01 11 00 02 11 01 03 11 01 FF C4 00 1D 00 01 00 02 02 03 01 01 00 00 00 00 00 00 00 00 00 00 07 08 03 06 04 05 09 02 01 FF C4 00 14 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 0C 03 01 00 02 10 03 10 00 00 01 B5 04 66 77 86 E2 00 00 00 0E 19 E7 81 24 17 4C 00 00 00 1C 63 CE F3 E0 BF 66 C2 00 00 00 0A E6 54 23 71 2E D1 BF 00 0E B4 EB CD 88 03 E0 82 CA 78 73 CB 92 4B C6 43 19 49 8E 80 BE A6 70 01 14 14 B0 D7 C9 DC B4 E6 33 CE D2 77 2E 51 FA 00 06 B0 55 82 03 3B 03 77 23 22 F7 93 08 00 00 63 22 C2 B5 11 31 B2 1E 84 9D C0 00 00 01 8C D3 CE 59 B4 80 7F FF C4 00 24 10 00 02 02 02 01 04 01 05 00 00 00 00 00 00 00 00 04 05 02 03 01 06 07 00 12 20 30 14 10 11 15 21 31 FF DA 00 08 01 01 00 01 05 02 CF EB A7 7B A8 AA 2D 45 B1 50 EA BF 51 72 ED 1B 6E 2E 64 37 E2 C3 67 86 3E A2 23 DF 46 E4 2C 85 73 A9 39 FC 33 45 0E 87 6E 3F AB 91 74 DC 9F 1B C7 98 B6 6B BB 51 29 2F D6 36 AA 1F 0D E0 63 0A 41 88 4F 06 3A 5F 59 43 13 8E E3 C7 D5 32 AD 9A BB 96 5E 8D F5 E9 89 D6 37 F1 5A D5 0B 23 6C 7A 9C FB 21 C8 5B 4D A5 1F A2 B2 27 0D E8 FB E6 AF 0D BF 4C A5 E5 0E 75 F2 54 5F 49 16 0D 3D 53 92 2E 02 4A B6 50 9B 57 B4 30 C0 09 D8 11 92 8B E2 A4 BF 24 DF E7 93 7D 7C 47 15 6D 7C 6B 68 3D 5C 3D 83 4C 16 E4 AF 99 FB C9 6C 57 47 1D F3 E3 55 9F 0D 37 9C EB 8D 91 7D A0 04 DB 0F 78 E0 D5 D9 BC 1B 86 96 B6 92 E6 8C 14 87 80 40 F4 CE B8 D9 83 F5 25 E7 E5 66 BA 1A BF 0F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 FF DA 00 08 01 03 01 01 3F 01 6B FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 FF DA 00 08 01 02 01 01 3F 01 6B FF C4 00 27 10 00 02 02 01 02 05 03 05 00 00 00 00 00 00 00 00 01 02 00 03 11 12 51 04 20 21 30 41 13 31 61 10 14 22 23 71 FF DA 00 08 01 01 00 06 3F 02 9A 19 BA CC D6 D9 ED D8 7E 25 DA 8F B1 9E 96 7A 76 DC 6E 25 C0 8F 32 BB 4F B6 62 D9 53 82 76 ED 9E 2A 85 FC A1 57 05 48 8A 55 CE 8D A2 E1 87 A9 B7 2E 6D 60 26 2B 70 79 08 61 91 1A EE 1D 71 67 C4 6A ED 52 31 16 CA D8 E3 68 A9 6B 68 B2 6A 53 91 F4 2C 7C 47 A6 B7 C2 8D A5 6A 1C 90 62 E7 6E 56 74 50 2D 8C B6 21 C6 F3 52 31 53 16 AE 24 EA 48 0D 76 8C 9F 12 EB 33 E2 59 61 F2 67 AE C3 A2 F3 95 B6 B1 9D E3 5B C3 0D 49 0A BA 95 22 06 AA C2 B3 ED AD 39 9F D8 AF 8E AD D8 D2 C3 22 16 55 08 F0 9A D7 5A CD 2E 84 19 5A 84 3A 73 2A AB 61 DA C3 0C 89 97 A4 66 7E AA C0 3C 9F FF C4 00 1F 10 01 00 02 02 02 03 01 01 00 00 00 00 00 00 00 00 01 00 11 21 31 41 51 20 30 61 10 81 FF DA 00 08 01 01 00 01 3F 21 54 57 44 AC 5F 98 30 8B D6 C7 6C 72 DA 51 6A 1B 4E FC 7A FE F6 92 93 56 D2 0E 25 55 C6 34 0C DB D5 B9 7C 03 94 08 F5 CB 98 B0 16 CD A3 50 AB 9B 78 FF 00 63 56 5F BB F1 9B FD 2C 8B B1 83 3B 75 08 9D 55 B4 81 8C 1C DA 0D BA 6B 2C 20 37 E4 FC 2D 50 5C 5A EB A6 D1 52 76 4B 89 3D BC 1B 87 45 9B 08 79 C5 C5 21 81 1E 98 D8 75 DB 0C 77 98 C3 01 B5 83 1C CB 5D 96 57 64 14 03 47 95 A2 B3 84 4D 2D D4 47 04 5C 92 80 77 17 2C B4 77 71 04 E5 40 66 87 E8 64 27 E1 8E 6C F4 47 7C 7D 11 A8 F7 64 49 60 55 A8 49 2A BB F5 21 0F 52 4A 2D FC 13 3B DE 2B C3 FF DA 00 0C 03 01 00 02 00 03 00 00 00 10 82 49 24 92 49 24 12 49 24 92 49 24 92 49 20 02 09 24 00 49 00 90 48 20 92 09 20 10 09 24 92 09 04 12 49 24 80 40 24 92 49 24 10 49 FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 FF DA 00 08 01 03 01 01 3F 10 6B FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 60 FF DA 00 08 01 02 01 01 3F 10 6B FF C4 00 21 10 01 01 01 00 02 01 04 03 01 00 00 00 00 00 00 00 01 11 00 21 31 30 10 20 61 91 41 51 71 A1 FF DA 00 08 01 01 00 01 3F 10 17 50 2A EE 20 A6 21 E7 28 3A 74 3C F8 D0 38 B1 F5 A4 8C 01 7A E7 3E 5A 91 38 E8 F1 38 BD FF 00 81 94 81 60 92 F3 94 58 87 F3 71 E0 9A 22 8C F1 20 11 E9 D1 D5 5E 74 E6 BA 61 04 C0 EF 9A A9 30 4D 46 C0 57 DA BC A8 59 0C B2 9B 20 38 40 23 47 D4 92 2C 15 13 14 94 2F 98 B9 59 30 20 1C 7E 5E 82 89 A7 BE 93 2A E3 B5 94 5A 7A 37 40 F6 BF 19 3F 7F 80 C3 D2 62 24 EF 77 99 0B F5 EC 40 22 51 FC 39 E5 01 39 0B 91 A4 CA 24 C9 75 68 B1 AE 6C 14 DE 30 C8 81 96 DC 15 02 27 E3 26 2B 03 FD C0 8C 8A 29 8C 44 08 07 B9 40 14 04 DB B9 BA 98 EF 1A 7D 70 8C 66 A1 3B 0E 65 51 B9 7D B1 AE A1 3E DD 08 70 D4 E7 C0 4E 46 23 4D 7F 0A 88 1C E4 3B 4A 73 71 99 5E 44 5C B6 A8 6E 4B 8D F3 00 3F 73 C4 D9 F1 1A 8C E9 11 AA 3A 38 AF 51 DD 7A FF 00 FF D9"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (Bytes.Encode.sequence [] |> Bytes.Encode.encode)


pdf : Bytes
pdf =
    Pdf.pdf
        { title = "Example pdf"
        , images =
            Dict.fromList
                [ ( "image", { size = ( Pixels.pixels 100, Pixels.pixels 200 ), jpgData = imageData } )
                , ( "image2", { size = ( Pixels.pixels 103, Pixels.pixels 70 ), jpgData = image2Data } )
                ]
        , pages =
            [ Pdf.page
                { size = Pdf.paperSize Portrait A4
                , contents =
                    [ Pdf.text
                        (Length.points 32)
                        (Pdf.timesRoman { bold = False, italic = False })
                        Point2d.origin
                        "Test\nnextline"
                    , Pdf.jpgImage
                        (BoundingBox2d.from
                            (Point2d.xy (Length.points 0) (Length.points 400))
                            (Point2d.xy (Length.points 100) (Length.points 200))
                        )
                        "image"
                    , Pdf.jpgImage
                        (BoundingBox2d.from
                            (Point2d.xy (Length.points 50) (Length.points 400))
                            (Point2d.xy (Length.points 153) (Length.points 470))
                        )
                        "image2"
                    , Pdf.text
                        (Length.points 32)
                        (Pdf.helvetica { bold = False, oblique = True })
                        (Point2d.fromTuple Length.points ( 0, 0 ))
                        "Test2"
                    ]
                }
            , Pdf.page
                { size = Pdf.paperSize Landscape A6
                , contents =
                    [ Pdf.text
                        (Length.points 32)
                        (Pdf.timesRoman { bold = False, italic = False })
                        Point2d.origin
                        "Test\nnext"
                    , Pdf.jpgImage
                        (BoundingBox2d.from
                            (Point2d.xy (Length.points 0) (Length.points 0))
                            (Point2d.xy (Length.points 103) (Length.points 70))
                        )
                        "image2"
                    , Pdf.text
                        (Length.points 32)
                        (Pdf.helvetica { bold = False, oblique = True })
                        (Point2d.fromTuple Length.points ( 0, 0 ))
                        "Test2"
                    ]
                }
            ]
        }
        |> Pdf.encoder
        |> Bytes.Encode.encode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Download ->
            ( model, File.Download.bytes "Example pdf.pdf" "application/pdf" pdf )


view model =
    Html.button [ Html.Events.onClick Download ] [ Html.text "Download" ]


main : Platform.Program () () Msg
main =
    Browser.element
        { init = always ( (), Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
