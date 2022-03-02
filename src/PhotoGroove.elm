module PhotoGroove exposing (main, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, input, label, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ImgClick thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (SizeClick size)
            , checked (selectedSize == size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loading ->
                [ h1 [] [ text "Loading..." ] ]

            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Errored errorMsg ->
                [ text ("Error: " ++ errorMsg) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , button [ onClick SurpriseClick ] [ text "Surprise me" ]
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
    ]


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


type Msg
    = ImgClick String
    | SurpriseClick
    | SizeClick ThumbnailSize
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImgClick url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        SurpriseClick ->
            case model.status of
                Loaded (firstPhoto :: rest) _ ->
                    Random.uniform firstPhoto rest
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        SizeClick size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok responseStr) ->
            case String.split "," responseStr of
                (firstUrl :: _) as urls ->
                    let
                        photos =
                            List.map Photo urls
                    in
                    ( { model | status = Loaded photos firstUrl }, Cmd.none )

                [] ->
                    ( { model | status = Errored "No photos found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!!" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
