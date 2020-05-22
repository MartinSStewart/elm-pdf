module Main exposing (..)

import Browser
import Bytes.Encode
import File.Download
import Html
import Html.Events
import Length
import Pdf
import Point2d
import Vector2d


type alias Model =
    ()


type Msg
    = Download


pdf =
    Pdf.pdf "Example pdf"
        [ Pdf.page
            { size = Vector2d.fromTuple Length.points ( 200, 300 )
            , contents =
                [ Pdf.text
                    (Length.points 32)
                    (Pdf.timesRoman { bold = False, italic = False })
                    Point2d.origin
                    "Test"
                , Pdf.text
                    (Length.points 32)
                    (Pdf.helvetica { bold = True, oblique = False })
                    (Point2d.fromTuple Length.points ( 0, 32 ))
                    "Test2"
                ]
            }
        ]
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
