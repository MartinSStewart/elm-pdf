module Rc4_2 exposing (decrypt)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE


type alias SBox =
    Array Int


getAt : Int -> Array a -> Maybe a
getAt idx xs =
    Array.get idx xs


setAt : Int -> a -> Array a -> Array a
setAt index value array =
    Array.set index value array


initialSBox : Array Int
initialSBox =
    List.range 0 255 |> Array.fromList


swap : Int -> Int -> SBox -> SBox
swap i j s =
    let
        si =
            getAt i s |> Maybe.withDefault 0

        sj =
            getAt j s |> Maybe.withDefault 0

        s1 =
            setAt j si s

        s2 =
            setAt i sj s1
    in
    s2


initialize : Array Int -> SBox
initialize key =
    let
        keyLength =
            Array.length key

        loop i j s =
            if i <= 255 then
                let
                    si =
                        getAt i s |> Maybe.withDefault 0

                    kj =
                        key |> getAt (modBy keyLength i) |> Maybe.withDefault 0

                    s1 =
                        swap i (modBy 256 (j + si + kj)) s
                in
                loop (i + 1) (modBy 256 (j + si + kj)) s1

            else
                s
    in
    loop 0 0 initialSBox


emptyBytes : Bytes
emptyBytes =
    BE.sequence [] |> BE.encode


decrypt : Bytes -> Bytes -> Bytes
decrypt key ciphertext =
    BD.decode (decoder key (Bytes.width ciphertext)) ciphertext |> Maybe.withDefault emptyBytes


bytesToInt : Bytes -> Array Int
bytesToInt bytes =
    BD.decode
        (BD.loop
            ( Bytes.width bytes, Array.empty )
            (\( remaining, array ) ->
                if remaining <= 0 then
                    BD.Done array |> BD.succeed

                else
                    BD.unsignedInt8 |> BD.map (\int -> ( remaining - 1, Array.push int array ) |> BD.Loop)
            )
        )
        bytes
        |> Maybe.withDefault Array.empty


decoder : Bytes -> Int -> BD.Decoder Bytes
decoder key width =
    BD.loop
        { newBytes = BE.sequence [], i = 0, j = 0, s = initialize (bytesToInt key), index = 0 }
        (\{ newBytes, i, j, s, index } ->
            if index >= width then
                BE.encode newBytes |> BD.Done |> BD.succeed

            else
                BD.map
                    (\c ->
                        let
                            i1 =
                                modBy 256 (i + 1)

                            si =
                                getAt i1 s |> Maybe.withDefault 0

                            j1 =
                                modBy 256 (j + si)

                            sj =
                                getAt j1 s |> Maybe.withDefault 0

                            s1 =
                                swap i1 j1 s

                            k =
                                Bitwise.xor c (getAt (modBy 256 (si + sj)) s1 |> Maybe.withDefault 0)
                        in
                        { newBytes =
                            BE.sequence
                                [ newBytes
                                , BE.unsignedInt8 k
                                ]
                        , i = i1
                        , j = j1
                        , s = s1
                        , index = index + 1
                        }
                            |> BD.Loop
                    )
                    BD.unsignedInt8
        )
