module Rc4 exposing (decrypt, encrypt)

import Bitwise


type alias SBox =
    List Int


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


setAt : Int -> a -> List a -> List a
setAt index value =
    updateAt index (always value)


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            tail : List a
            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                List.take index list ++ fn x :: xs

            [] ->
                list


initialSBox : List Int
initialSBox =
    List.range 0 255


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


initialize : String -> SBox
initialize key =
    let
        keyLength =
            String.length key

        loop i j s =
            if i <= 255 then
                let
                    si =
                        getAt i s |> Maybe.withDefault 0

                    kj =
                        String.toList key |> getAt (modBy keyLength i) |> Maybe.withDefault ' ' |> Char.toCode

                    s1 =
                        swap i (modBy 256 (j + si + kj)) s
                in
                loop (i + 1) (modBy 256 (j + si + kj)) s1

            else
                s
    in
    loop 0 0 initialSBox


encryptHelper : Int -> Int -> SBox -> String -> List Int
encryptHelper i j s plaintext =
    if String.isEmpty plaintext then
        []

    else
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
                String.toList plaintext |> getAt 0 |> Maybe.withDefault ' ' |> Char.toCode

            c =
                Bitwise.xor k (getAt (modBy 256 (si + sj)) s1 |> Maybe.withDefault 0)
        in
        c :: encryptHelper i1 j1 s1 (String.dropLeft 1 plaintext)


encrypt : String -> String -> List Int
encrypt key plaintext =
    let
        s =
            initialize key
    in
    encryptHelper 0 0 s plaintext


decrypt : String -> List Int -> String
decrypt key ciphertext =
    let
        s =
            initialize key
    in
    decryptHelper 0 0 s ciphertext


decryptHelper : Int -> Int -> List Int -> List Int -> String
decryptHelper i j s ciphertext =
    if List.isEmpty ciphertext then
        ""

    else
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

            c =
                getAt 0 ciphertext |> Maybe.withDefault 0

            k =
                Bitwise.xor c (getAt (modBy 256 (si + sj)) s1 |> Maybe.withDefault 0) |> Char.fromCode
        in
        String.fromChar k ++ decryptHelper i1 j1 s1 (List.drop 1 ciphertext)
