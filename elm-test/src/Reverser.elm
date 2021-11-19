module Reverser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { text : String
    , reverse : String
    }


type Msg
    = Update String


init : Model
init =
    Model "" ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update string ->
            { model | text = string, reverse = String.reverse string }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "text to reverse", value model.text, onInput Update ] []
        , div []
            [ span [] [ text model.reverse ]
            ]
        ]
