module Counter exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events


type Msg
    = Increment
    | Decrement
    | Reset
    | UpdateIncrementStep Int


type alias Model =
    { count : Int, step : Int }


init : Model
init =
    Model 0 1


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + model.step }

        Decrement ->
            { model | count = model.count - model.step }

        Reset ->
            { model | count = 0, step = 1 }

        UpdateIncrementStep step ->
            { model | step = step }


changeIncrementStep : String -> Msg
changeIncrementStep string =
    UpdateIncrementStep (Maybe.withDefault 1 (String.toInt string))


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text (String.fromInt model.count) ]
        , Html.input [ Events.onInput changeIncrementStep, Attr.value (String.fromInt model.step) ] []
        , Html.button [ Events.onClick Decrement ] [ Html.text "decrease" ]
        , Html.button [ Events.onClick Increment ] [ Html.text "increase" ]
        , Html.button [ Events.onClick Reset ] [ Html.text "reset" ]
        ]
