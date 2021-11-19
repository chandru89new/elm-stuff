module RandomCat exposing (..)

import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D


type Model
    = Initial
    | Loading
    | Success String
    | Error ()


type Msg
    = SendRequest
    | GotResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SendRequest ->
            ( Loading
            , Http.get
                { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
                , expect = Http.expectJson GotResponse (D.field "data" (D.field "image_url" D.string))
                }
            )

        GotResponse response ->
            case response of
                Ok gif ->
                    ( Success gif, Cmd.none )

                Err _ ->
                    ( Error (), Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Initial ->
            div []
                [ button [ onClick SendRequest ] [ text "Random cat" ]
                ]

        Loading ->
            div [] [ text "Loading..." ]

        Success gif ->
            div []
                [ img [ style "display" "block", src gif ] []
                , button [ onClick SendRequest ] [ text "Random cat" ]
                ]

        Error _ ->
            div []
                [ div [] [ text "Could not load" ]
                , button [ onClick SendRequest ] [ text "Random cat" ]
                ]
