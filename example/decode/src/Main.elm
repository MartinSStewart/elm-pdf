module Main exposing (main)

import Browser
import Html exposing (Html)
import Http exposing (Error(..), Response(..))
import Pdf exposing (ASizes(..), DecodedPdf, Orientation(..), PageCoordinates, Pdf)


type Msg
    = LoadedPdf (Result Http.Error (Result String DecodedPdf))


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
    , Http.get
        { url = "https://files.slack.com/files-pri/T05MF23NJSK-F05V1JPC48P/download/sample_energy_report.pdf?origin_team=T05MF23NJSK"
        , expect = Http.expectBytes LoadedPdf Pdf.decoder
        }
    )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
