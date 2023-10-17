module Main exposing (main)

import Browser
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pdf exposing (ASizes(..), DecodedPdf, Orientation(..), PageCoordinates, Pdf)
import Task


type Msg
    = PressedSelectPdf
    | SelectedPdf File
    | LoadedPdf (Result String DecodedPdf)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedSelectPdf ->
            ( model, File.Select.file [ "application/pdf" ] SelectedPdf )

        SelectedPdf file ->
            ( model, File.toBytes file |> Task.perform (\bytes -> Pdf.fromBytes bytes |> LoadedPdf) )

        LoadedPdf result ->
            ( { model | output = result }, Cmd.none )


view : Model -> Html Msg
view { output } =
    Html.div
        [ Html.Attributes.style "padding" "32px" ]
        [ Html.button [ Html.Events.onClick PressedSelectPdf ] [ Html.text "Select a PDF file to parse" ]
        , Html.div
            []
            [ case output of
                Ok ok ->
                    Html.div
                        []
                        [ Html.div [] [ Html.text (Debug.toString ok.metadata) ]
                        , Html.br [] []
                        , List.map (\result -> Html.div [] [ Debug.toString result |> Html.text ]) ok.sections
                            |> List.intersperse (Html.br [] [])
                            |> Html.div []
                        ]

                Err error ->
                    Html.text error
            ]
        ]


type alias Model =
    { output : Result String DecodedPdf }


init : () -> ( Model, Cmd Msg )
init () =
    ( { output = Err "" }, Cmd.none )


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
