module Clock exposing (..)

import Html
import Html.Attributes as Attr
import Json.Decode exposing (int)
import Task
import Time


type Msg
    = Tick Time.Posix
    | SetTimezone Time.Zone


type alias Model =
    { time : { h : Int, m : Int, s : Int }
    , zone : Time.Zone
    }


init =
    Model { h = 0, m = 0, s = 0 } Time.utc


update msg model =
    case msg of
        Tick posix ->
            let
                h =
                    prependZero (Time.toHour model.zone posix)

                m =
                    prependZero (Time.toMinute model.zone posix)

                s =
                    prependZero (Time.toSecond model.zone posix)
            in
            { model | time = combineNeatly h m s }

        SetTimezone zone ->
            { model | zone = zone }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ mainCSS
        , Html.div []
            [ Html.div
                [ Attr.class "second-hand"
                , Attr.style "transform" ("rotate(" ++ String.fromInt (model.time.s * 6 + -90) ++ "deg)")
                ]
                []
            ]
        ]


subscriptions _ =
    Time.every 1000 Tick


setTimezone =
    Task.perform SetTimezone Time.here


prependZero : Int -> Int
prependZero int =
    int



-- prependZero int =
--     if int < 10 then
--         "0" ++ String.fromInt int
--     else
--         String.fromInt int


combineNeatly h m s =
    { h = h, m = m, s = s }


mainCSS =
    let
        customStyles =
            """
      .second-hand {
        transition: all 0.2 ease-in-out;
        height: 2px;
        width: 50px;
        background: black;
        transform-origin: top left;
        margin-left: 100px;
      }
    """
    in
    Html.node "style" [] [ Html.text customStyles ]
