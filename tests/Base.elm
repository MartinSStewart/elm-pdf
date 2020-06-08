module Base exposing (tests)

import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Expect exposing (Expectation)
import Hex.Convert
import Length
import Pdf
import Pixels
import Point2d
import Test exposing (Test, describe, test)
import Vector2d


jpegImage =
    "FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48 00 48 00 00 FF FE 00 13 43 72 65 61 74 65 64 20 77 69 74 68 20 47 49 4D 50 FF DB 00 43 00 03 02 02 03 02 02 03 03 03 03 04 03 03 04 05 08 05 05 04 04 05 0A 07 07 06 08 0C 0A 0C 0C 0B 0A 0B 0B 0D 0E 12 10 0D 0E 11 0E 0B 0B 10 16 10 11 13 14 15 15 15 0C 0F 17 18 16 14 18 12 14 15 14 FF DB 00 43 01 03 04 04 05 04 05 09 05 05 09 14 0D 0B 0D 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 FF C2 00 11 08 00 04 00 05 03 01 11 00 02 11 01 03 11 01 FF C4 00 14 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 08 FF C4 00 14 01 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 0C 03 01 00 02 10 03 10 00 00 01 54 9F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 05 02 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 03 01 01 3F 01 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 02 01 01 3F 01 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 06 3F 02 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 3F 21 7F FF DA 00 0C 03 01 00 02 00 03 00 00 00 10 9F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 03 01 01 3F 10 7F FF C4 00 14 11 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 02 01 01 3F 10 7F FF C4 00 14 10 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF DA 00 08 01 01 00 01 3F 10 7F FF D9"
        |> String.filter Char.isAlphaNum
        |> Hex.Convert.toBytes
        |> Maybe.withDefault (BE.encode (BE.sequence []))


tests =
    describe "tests"
        [ test "Get jpeg dimensions" <|
            \_ ->
                case Pdf.jpeg "ImageId" jpegImage of
                    Just jpeg ->
                        Pdf.imageSize jpeg |> Expect.equal ( Pixels.pixels 5, Pixels.pixels 4 )

                    Nothing ->
                        Expect.fail "Failed to parse jpeg."
        ]
