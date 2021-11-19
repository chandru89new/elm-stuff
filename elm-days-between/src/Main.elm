port module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = SetStartDate String
    | SetEndDate String
    | CheckDaysBetween
    | PrintDaysBetween Res


type alias Model =
    { startDate : String
    , endDate : String
    , diffString : String
    , error : String
    }


type alias Res =
    { data : String
    , error : String
    }


port getDaysBetweenFromDateFns : (Res -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ getDaysBetweenFromDateFns PrintDaysBetween
        ]


port sendDatesToDateFns : Model -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "2021-01-01" "2021-01-31" "" "", Cmd.none )


view : Model -> H.Html Msg
view model =
    H.div [ Attr.class "p-10 w-1/3" ]
        [ H.div []
            [ H.input
                [ Attr.placeholder "Type start date"
                , Attr.autofocus True
                , Ev.onInput SetStartDate
                ]
                []
            , H.input
                [ Attr.placeholder "Type end date"
                , Ev.onInput SetEndDate
                ]
                []
            ]
        , H.div [] [ H.text "All of the dates can be in many formats" ]
        , H.button [ Ev.onClick CheckDaysBetween ] [ H.text "How many days between?" ]
        , H.div [] [ H.text model.diffString ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckDaysBetween ->
            ( model, sendDatesToDateFns model )

        SetStartDate str ->
            ( { model | startDate = str }, Cmd.none )

        SetEndDate str ->
            ( { model | endDate = str }, Cmd.none )

        PrintDaysBetween data ->
            if data.error /= "" then
                ( { model | error = data.error }, Cmd.none )

            else
                ( { model | diffString = data.data }, Cmd.none )
