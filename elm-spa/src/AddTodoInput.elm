module AddTodoInput exposing (..)

import Browser.Dom as Dom
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Decoder
import Task


type alias Model =
    { todoText : String
    , state : InputState
    }


init =
    Model "" ShowLink


type Msg
    = UpdateTodoText String
    | ToggleState (Maybe InputState)
    | AddTodo String
    | NoOp


type InputState
    = ShowLink
    | ShowInput


type alias Todo =
    { todo : String
    , projectId : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTodoText str ->
            ( { model | todoText = str }, Cmd.none )

        ToggleState state ->
            let
                task =
                    Dom.focus "add-todo-input"
            in
            case state of
                Nothing ->
                    ( { model
                        | state =
                            if model.state == ShowLink then
                                ShowInput

                            else
                                ShowLink
                      }
                    , Task.attempt (\_ -> NoOp) task
                    )

                Just st ->
                    ( { model | state = st }, Task.attempt (\_ -> NoOp) task )

        AddTodo _ ->
            ( { model | state = ShowLink, todoText = "" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> H.Html Msg
view model =
    case model.state of
        ShowInput ->
            H.div
                [ Attr.class "flex items-center justify-between"
                ]
                [ H.input
                    [ Attr.placeholder "Enter todo here"
                    , Attr.autofocus True
                    , Ev.onInput UpdateTodoText
                    , Attr.value model.todoText
                    , Attr.id "add-todo-input"
                    , Ev.on "keyup"
                        (Decoder.map
                            (\code ->
                                if code == 13 then
                                    AddTodo model.todoText

                                else
                                    NoOp
                            )
                            Ev.keyCode
                        )
                    ]
                    []
                , H.button
                    [ Ev.onClick (AddTodo model.todoText)
                    , Attr.disabled
                        (if String.trim model.todoText == "" then
                            True

                         else
                            False
                        )
                    ]
                    [ H.text "Add todo" ]
                ]

        ShowLink ->
            H.div []
                [ H.span
                    [ Attr.class "a"
                    , Ev.onClick (ToggleState Nothing)
                    , Attr.id "add-todo-link"
                    ]
                    [ H.text "+ Add todo" ]
                ]
