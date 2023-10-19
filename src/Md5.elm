module Md5 exposing (hex, byteValues, fromBytes)

{-| This library allows you to compute MD5 message digests in Elm. It exposes a
single function that takes any string and outputs a "fingerprint" containing 32
hexadecimal characters. More information about the MD5 algorithm can be found
[here](https://en.wikipedia.org/wiki/MD5).


# Digest Functions

@docs hex, byteValues, fromBytes

-}

import Array exposing (Array)
import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy, shiftRightZfBy)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


{-| Given a string of arbitrary length, returns a string of 32 hexadecimal
characters (a-f, 0-9) representing the 128-bit MD5 message digest.

    hex ""
    --> "d41d8cd98f00b204e9800998ecf8427e"

    hex "foobarbaz"
    --> "6df23dc03f9b54cc38a0fc1483df6e21"

-}
hex : String -> String
hex s =
    fromString s


{-| Given a string of arbitrary length, returns a list of integers representing
the hash as a series of individual bytes.

    byteValues "hello world"
    --> [ 0x5e , 0xb6 , 0x3b , 0xbb
    --> , 0xe0 , 0x1e , 0xee , 0xd0
    --> , 0x93 , 0xcb , 0x22 , 0xbb
    --> , 0x8f , 0x5a , 0xcd , 0xc3
    --> ]

-}
byteValues : String -> List Int
byteValues string =
    toByteValues (hashBytes (Encode.encode (Encode.string string)) initialHashState)


toByteValues : State -> List Int
toByteValues { a, b, c, d } =
    [ and a 255
    , and (shiftRightZfBy 8 a) 255
    , and (shiftRightZfBy 16 a) 255
    , and (shiftRightZfBy 24 a) 255
    , and b 255
    , and (shiftRightZfBy 8 b) 255
    , and (shiftRightZfBy 16 b) 255
    , and (shiftRightZfBy 24 b) 255
    , and c 255
    , and (shiftRightZfBy 8 c) 255
    , and (shiftRightZfBy 16 c) 255
    , and (shiftRightZfBy 24 c) 255
    , and d 255
    , and (shiftRightZfBy 8 d) 255
    , and (shiftRightZfBy 16 d) 255
    , and (shiftRightZfBy 24 d) 255
    ]


fromBytes : Bytes -> String
fromBytes bytes =
    let
        { a, b, c, d } =
            hashBytes bytes initialHashState
    in
    unsigned32ToHex a ++ unsigned32ToHex b ++ unsigned32ToHex c ++ unsigned32ToHex d


fromString : String -> String
fromString string =
    fromBytes (Encode.encode (Encode.string string))


reduceChunk ({ a, b, c, d } as acc) x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
    let
        s11 =
            7

        s12 =
            12

        s13 =
            17

        s14 =
            22

        s21 =
            5

        s22 =
            9

        s23 =
            14

        s24 =
            20

        s31 =
            4

        s32 =
            11

        s33 =
            16

        s34 =
            23

        s41 =
            6

        s42 =
            10

        s43 =
            15

        s44 =
            21

        a00 =
            a

        b00 =
            b

        c00 =
            c

        d00 =
            d

        a01 =
            ff a00 b00 c00 d00 x0 s11 0xD76AA478

        d01 =
            ff d00 a01 b00 c00 x1 s12 0xE8C7B756

        c01 =
            ff c00 d01 a01 b00 x2 s13 0x242070DB

        b01 =
            ff b00 c01 d01 a01 x3 s14 0xC1BDCEEE

        a02 =
            ff a01 b01 c01 d01 x4 s11 0xF57C0FAF

        d02 =
            ff d01 a02 b01 c01 x5 s12 0x4787C62A

        c02 =
            ff c01 d02 a02 b01 x6 s13 0xA8304613

        b02 =
            ff b01 c02 d02 a02 x7 s14 0xFD469501

        a03 =
            ff a02 b02 c02 d02 x8 s11 0x698098D8

        d03 =
            ff d02 a03 b02 c02 x9 s12 0x8B44F7AF

        c03 =
            ff c02 d03 a03 b02 x10 s13 0xFFFF5BB1

        b03 =
            ff b02 c03 d03 a03 x11 s14 0x895CD7BE

        a04 =
            ff a03 b03 c03 d03 x12 s11 0x6B901122

        d04 =
            ff d03 a04 b03 c03 x13 s12 0xFD987193

        c04 =
            ff c03 d04 a04 b03 x14 s13 0xA679438E

        b04 =
            ff b03 c04 d04 a04 x15 s14 0x49B40821

        a05 =
            gg a04 b04 c04 d04 x1 s21 0xF61E2562

        d05 =
            gg d04 a05 b04 c04 x6 s22 0xC040B340

        c05 =
            gg c04 d05 a05 b04 x11 s23 0x265E5A51

        b05 =
            gg b04 c05 d05 a05 x0 s24 0xE9B6C7AA

        a06 =
            gg a05 b05 c05 d05 x5 s21 0xD62F105D

        d06 =
            gg d05 a06 b05 c05 x10 s22 0x02441453

        c06 =
            gg c05 d06 a06 b05 x15 s23 0xD8A1E681

        b06 =
            gg b05 c06 d06 a06 x4 s24 0xE7D3FBC8

        a07 =
            gg a06 b06 c06 d06 x9 s21 0x21E1CDE6

        d07 =
            gg d06 a07 b06 c06 x14 s22 0xC33707D6

        c07 =
            gg c06 d07 a07 b06 x3 s23 0xF4D50D87

        b07 =
            gg b06 c07 d07 a07 x8 s24 0x455A14ED

        a08 =
            gg a07 b07 c07 d07 x13 s21 0xA9E3E905

        d08 =
            gg d07 a08 b07 c07 x2 s22 0xFCEFA3F8

        c08 =
            gg c07 d08 a08 b07 x7 s23 0x676F02D9

        b08 =
            gg b07 c08 d08 a08 x12 s24 0x8D2A4C8A

        a09 =
            hh a08 b08 c08 d08 x5 s31 0xFFFA3942

        d09 =
            hh d08 a09 b08 c08 x8 s32 0x8771F681

        c09 =
            hh c08 d09 a09 b08 x11 s33 0x6D9D6122

        b09 =
            hh b08 c09 d09 a09 x14 s34 0xFDE5380C

        a10 =
            hh a09 b09 c09 d09 x1 s31 0xA4BEEA44

        d10 =
            hh d09 a10 b09 c09 x4 s32 0x4BDECFA9

        c10 =
            hh c09 d10 a10 b09 x7 s33 0xF6BB4B60

        b10 =
            hh b09 c10 d10 a10 x10 s34 0xBEBFBC70

        a11 =
            hh a10 b10 c10 d10 x13 s31 0x289B7EC6

        d11 =
            hh d10 a11 b10 c10 x0 s32 0xEAA127FA

        c11 =
            hh c10 d11 a11 b10 x3 s33 0xD4EF3085

        b11 =
            hh b10 c11 d11 a11 x6 s34 0x04881D05

        a12 =
            hh a11 b11 c11 d11 x9 s31 0xD9D4D039

        d12 =
            hh d11 a12 b11 c11 x12 s32 0xE6DB99E5

        c12 =
            hh c11 d12 a12 b11 x15 s33 0x1FA27CF8

        b12 =
            hh b11 c12 d12 a12 x2 s34 0xC4AC5665

        a13 =
            ii a12 b12 c12 d12 x0 s41 0xF4292244

        d13 =
            ii d12 a13 b12 c12 x7 s42 0x432AFF97

        c13 =
            ii c12 d13 a13 b12 x14 s43 0xAB9423A7

        b13 =
            ii b12 c13 d13 a13 x5 s44 0xFC93A039

        a14 =
            ii a13 b13 c13 d13 x12 s41 0x655B59C3

        d14 =
            ii d13 a14 b13 c13 x3 s42 0x8F0CCC92

        c14 =
            ii c13 d14 a14 b13 x10 s43 0xFFEFF47D

        b14 =
            ii b13 c14 d14 a14 x1 s44 0x85845DD1

        a15 =
            ii a14 b14 c14 d14 x8 s41 0x6FA87E4F

        d15 =
            ii d14 a15 b14 c14 x15 s42 0xFE2CE6E0

        c15 =
            ii c14 d15 a15 b14 x6 s43 0xA3014314

        b15 =
            ii b14 c15 d15 a15 x13 s44 0x4E0811A1

        a16 =
            ii a15 b15 c15 d15 x4 s41 0xF7537E82

        d16 =
            ii d15 a16 b15 c15 x11 s42 0xBD3AF235

        c16 =
            ii c15 d16 a16 b15 x2 s43 0x2AD7D2BB

        b16 =
            ii b15 c16 d16 a16 x9 s44 0xEB86D391

        a17 =
            addUnsigned a00 a16

        b17 =
            addUnsigned b00 b16

        c17 =
            addUnsigned c00 c16

        d17 =
            addUnsigned d00 d16
    in
    { a = a17, b = b17, c = c17, d = d17 }


padBuffer : Bytes -> Bytes
padBuffer bytes =
    let
        byteCount =
            Bytes.width bytes

        finalBlockSize =
            modBy 64 byteCount

        paddingSize =
            if finalBlockSize < 56 then
                55 - finalBlockSize

            else
                119 - finalBlockSize

        message =
            Encode.encode
                (Encode.sequence
                    [ Encode.bytes bytes
                    , Encode.unsignedInt8 0x80
                    , Encode.sequence (List.repeat paddingSize (Encode.unsignedInt8 0))
                    , Encode.unsignedInt32 LE (Bitwise.shiftLeftBy 3 byteCount)
                    , Encode.unsignedInt32 LE 0
                    ]
                )
    in
    message


hashBytes : Bytes -> State -> State
hashBytes bytes state =
    let
        message =
            padBuffer bytes

        numberOfChunks : Int
        numberOfChunks =
            Bytes.width message // 64

        hashState : Decoder State
        hashState =
            iterate numberOfChunks reduceBytesMessage state
    in
    case Decode.decode hashState message of
        Just newState ->
            newState

        Nothing ->
            state


u32 : Decoder Int
u32 =
    Decode.unsignedInt32 LE


reduceBytesMessage : State -> Decoder State
reduceBytesMessage state =
    map16 (reduceChunk state) u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32 u32


type alias State =
    { a : Int, b : Int, c : Int, d : Int }


initialHashState : State
initialHashState =
    State 0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476


rotateLeft : Int -> Int -> Int
rotateLeft bits input =
    or (shiftLeftBy bits input) (shiftRightZfBy (32 - bits) input)


addUnsigned : Int -> Int -> Int
addUnsigned x y =
    (x + y) |> Bitwise.and 0xFFFFFFFF


ff : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
ff a b c d x s ac =
    let
        f =
            Bitwise.xor c d
                |> Bitwise.and b
                |> Bitwise.xor d
    in
    rotateLeft s (f + x + ac + a) + b


gg : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
gg a b c d x s ac =
    let
        g =
            Bitwise.xor b c
                |> Bitwise.and d
                |> Bitwise.xor c
    in
    rotateLeft s (g + x + ac + a) + b


hh : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
hh a b c d x s ac =
    let
        h =
            Bitwise.xor b c |> Bitwise.xor d
    in
    rotateLeft s (h + x + ac + a) + b


ii : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
ii a b c d x s ac =
    let
        i =
            Bitwise.xor c (or b (complement d))
                -- complement can flip the sign. Force unsigned
                |> Bitwise.shiftRightZfBy 0
    in
    rotateLeft s (i + x + ac + a) + b



-- BYTES HELPERS


{-| The most efficient implmenentation for `map16`, given that `Decode.map5` is the highest defined in Kernel code
-}
map16 :
    (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> b8 -> b9 -> b10 -> b11 -> b12 -> b13 -> b14 -> b15 -> b16 -> result)
    -> Decoder b1
    -> Decoder b2
    -> Decoder b3
    -> Decoder b4
    -> Decoder b5
    -> Decoder b6
    -> Decoder b7
    -> Decoder b8
    -> Decoder b9
    -> Decoder b10
    -> Decoder b11
    -> Decoder b12
    -> Decoder b13
    -> Decoder b14
    -> Decoder b15
    -> Decoder b16
    -> Decoder result
map16 f b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 =
    let
        d1 =
            Decode.map4 (\a b c d -> f a b c d) b1 b2 b3 b4

        d2 =
            Decode.map5 (\h a b c d -> h a b c d) d1 b5 b6 b7 b8

        d3 =
            Decode.map5 (\h a b c d -> h a b c d) d2 b9 b10 b11 b12

        d4 =
            Decode.map5 (\h a b c d -> h a b c d) d3 b13 b14 b15 b16
    in
    d4


{-| Iterate a decoder `n` times
-}
iterate : Int -> (a -> Decoder a) -> a -> Decoder a
iterate n step initial =
    Decode.loop ( n, initial ) (loopHelp step)


loopHelp step ( n, state ) =
    if n > 0 then
        step state
            |> Decode.map (\new -> Loop ( n - 1, new ))

    else
        Decode.succeed (Decode.Done state)



-- HEX CONVERSION HELPERS


{-| Turn an integer into an 8-character hex string

The integer is treated as litte-endian

-}
unsigned32ToHex : Int -> String
unsigned32ToHex value =
    let
        p1 =
            value

        p2 =
            Bitwise.shiftRightZfBy 4 value

        p3 =
            Bitwise.shiftRightZfBy 8 value

        p4 =
            Bitwise.shiftRightZfBy 12 value

        p5 =
            Bitwise.shiftRightZfBy 16 value

        p6 =
            Bitwise.shiftRightZfBy 20 value

        p7 =
            Bitwise.shiftRightZfBy 24 value

        p8 =
            Bitwise.shiftRightZfBy 28 value
    in
    ""
        |> String.cons (unsafeToDigit (Bitwise.and p7 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p8 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p5 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p6 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p3 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p4 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p1 0x0F))
        |> String.cons (unsafeToDigit (Bitwise.and p2 0x0F))


{-| ONLY EVER CALL THIS WITH INTEGERS BETWEEN 0 and 15!
-}
unsafeToDigit : Int -> Char
unsafeToDigit num =
    case num of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            -- if this ever gets called with a number over 15, it will never
            -- terminate! If that happens, debug further by uncommenting this:
            --
            -- Debug.todo ("Tried to convert " ++ toString num ++ " to hexadecimal.")
            unsafeToDigit num
