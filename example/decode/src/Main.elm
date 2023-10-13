module Main exposing (main)

import Browser
import Html exposing (Html)
import Http exposing (Error(..), Response(..))
import Pdf exposing (ASizes(..), DecodedPdf, Orientation(..), PageCoordinates, Pdf)
import Task


type Msg
    = LoadedPdf (Result String DecodedPdf)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedPdf result ->
            let
                _ =
                    Debug.log "" result
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.text ""


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}
    , Http.task
        { method = "GET"
        , headers = []
        , body = Http.emptyBody
        , url = "https://raw.githubusercontent.com/MartinSStewart/elm-pdf/6f46a688f3301224e5c72635143d2338958924e8/example/decode/Sample%20Energy%20Report.pdf"
        , resolver =
            Http.bytesResolver
                (\response ->
                    case response of
                        BadUrl_ _ ->
                            Err "Bad url"

                        Timeout_ ->
                            Err "Timed out"

                        NetworkError_ ->
                            Err "Network error"

                        BadStatus_ metadata body ->
                            Err "Bad status"

                        GoodStatus_ metadata body ->
                            Pdf.fromBytes body
                )
        , timeout = Nothing
        }
        |> Task.attempt LoadedPdf
    )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
