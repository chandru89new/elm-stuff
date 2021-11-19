module Keydown exposing (..)

import Browser.Events exposing (onKeyDown)
import Json.Decode as D


type Msg
    = Keydown String


type alias Model =
    String


init : Model
init =
    ""


update : Msg -> Model -> Model
update msg _ =
    case msg of
        Keydown str ->
            case String.uncons str of
                Just ( _, "" ) ->
                    str

                _ ->
                    "Non-printing key"


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (D.map (\x -> Keydown x) (D.field "key" D.string))
