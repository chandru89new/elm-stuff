module Main exposing (..)

-- import Browser.Events exposing (onKeyDown, onKeyUp)

import Book
import Browser
import Clock
import Counter
import Html exposing (Html, div, span)
import Keydown
import RandomCat
import Reverser


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ReverserMsg Reverser.Msg
    | CounterMsg Counter.Msg
    | KeyDownMsg Keydown.Msg
    | BookMsg Book.Msg
    | RandomCatMsg RandomCat.Msg
    | ClockMsg Clock.Msg


type alias Model =
    { reverser : Reverser.Model
    , counter : Counter.Model
    , key : Keydown.Model
    , book : Book.Model
    , cat : RandomCat.Model
    , clock : Clock.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { reverser = Reverser.init
      , counter = Counter.init
      , key = Keydown.init
      , book = Book.init
      , cat = RandomCat.Initial
      , clock = Clock.init
      }
    , Cmd.batch
        [ Cmd.map ClockMsg Clock.setTimezone
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReverserMsg reverserMsg ->
            ( { model | reverser = Reverser.update reverserMsg model.reverser }, Cmd.none )

        CounterMsg counterMsg ->
            ( { model | counter = Counter.update counterMsg model.counter }, Cmd.none )

        KeyDownMsg keydownMsg ->
            ( { model | key = Keydown.update keydownMsg model.key }, Cmd.none )

        BookMsg bookMsg ->
            let
                ( f, s ) =
                    Book.update bookMsg model.book
            in
            ( { model | book = f }, Cmd.map BookMsg s )

        RandomCatMsg randomCatMsg ->
            let
                ( f, s ) =
                    RandomCat.update randomCatMsg model.cat
            in
            ( { model | cat = f }, Cmd.map RandomCatMsg s )

        ClockMsg clockMsg ->
            ( { model | clock = Clock.update clockMsg model.clock }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.map ReverserMsg (Reverser.view model.reverser)
        , Html.map CounterMsg (Counter.view model.counter)
        , div []
            [ span [] [ Html.text "Pressed key: " ]
            , span [] [ Html.text model.key ]
            ]
        , Html.map BookMsg (Book.view model.book)
        , Html.map RandomCatMsg (RandomCat.view model.cat)
        , Html.map ClockMsg (Clock.view model.clock)
        ]


subscriptions model =
    Sub.batch
        [ Sub.map KeyDownMsg (Keydown.subscriptions model.key)
        , Sub.map ClockMsg (Clock.subscriptions model.clock)
        ]
