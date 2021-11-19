module Book exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Http


type Model
    = Initial
    | Loading
    | Success String
    | Error String


type Msg
    = SendRequest
    | GotResponse (Result Http.Error String)


init =
    Initial


view : Model -> Html Msg
view model =
    case model of
        Initial ->
            div []
                [ button [ onClick SendRequest ] [ text "Load book" ]
                ]

        Loading ->
            div [] [ text "Loading..." ]

        Success str ->
            div [] [ text str ]

        Error err ->
            div [] [ text err ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SendRequest ->
            ( Loading
            , Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , expect = Http.expectString GotResponse
                }
            )

        GotResponse resp ->
            case resp of
                Ok str ->
                    ( Success str, Cmd.none )

                Err _ ->
                    ( Error "Could not load", Cmd.none )
