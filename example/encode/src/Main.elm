module Main exposing (main)

import BoundingBox2d
import Browser
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File.Download
import Html exposing (Html)
import Html.Events
import Http exposing (Error(..), Response(..))
import Length exposing (Length, Meters)
import Pdf exposing (ASizes(..), Orientation(..), PageCoordinates, Pdf)
import Point2d exposing (Point2d)
import Vector2d


type Msg
    = Download
    | LoadImage String (Result Http.Error Bytes)


titleFontSize : Length
titleFontSize =
    Length.points 100


normalFontSize : Length
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


defaultFont : Pdf.Font
defaultFont =
    Pdf.helvetica { bold = False, oblique = False }


margin : number
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


imagesToLoad : List (Cmd Msg)
imagesToLoad =
    [ getImage "pdfLogo" "https://cors-anywhere.herokuapp.com/https://fmfencing.com/images/stories/PDF-Icon.jpg"
    , getImage "butWhy" "https://cors-anywhere.herokuapp.com/https://i.ytimg.com/vi/3Z9yK3sMDUU/maxresdefault.jpg"
    , getImage "jpeg" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716251113011019776/jpeg.jpg"
    , getImage "deepfriedJpeg" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716251175271268352/deepfried_jpeg.jpg"
    , getImage "unicode" "https://cors-anywhere.herokuapp.com/https://cdn.discordapp.com/attachments/168212010817814528/716292238086242344/wave.jpg"
    ]


init : () -> ( Model, Cmd Msg )
init () =
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
