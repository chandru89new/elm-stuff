port module Main exposing (..)

-- import Time as T
-- import Debug

import AddTodoInput
import Browser as B
import Browser.Dom as Dom
import Browser.Navigation as BN
import Common exposing (..)
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Decoder
import Random
import Task
import Url as U
import Url.Parser as UP exposing ((</>))


main =
    B.application
        { init = init
        , view = view
        , update = update
        , onUrlChange = RouteChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }


type alias Model =
    { page : Page
    , navKey : BN.Key
    , projects : List Project
    , todos : List Todo
    , addTodoWidget : AddTodoInput.Model
    , addProjectWidget : AddProjectWidget
    }


type Page
    = NotFound
    | HomePage
    | ProjectPage String


type Msg
    = LinkClicked B.UrlRequest
    | RouteChanged U.Url
    | ToggleTodoState String
    | AddTodoWidget AddTodoInput.Msg
    | AddTodo Todo
    | ToggleProjectWidgetState ProjectWidgetState
    | AddProject Project
    | UpdateProjectNameInWidget String
    | GenerateNewProject
    | NoOp
    | ShowDeleteConfirmation String
    | ProcessDeleteTodoConfirmation ( Bool, String )
    | DeleteTodo String
    | HideInputs


type alias TodoJS =
    { id : String
    , todo : String
    , status : String
    , projectId : String
    }


type alias AddProjectWidget =
    { projectName : String
    , projectWidgetState : ProjectWidgetState
    }


type ProjectWidgetState
    = ShowLink
    | ShowInput


type alias Flags =
    { todos : List TodoJS
    , projects : List Project
    }


parseFlagForTodo : List TodoJS -> List Todo
parseFlagForTodo =
    List.map
        (\todo ->
            Todo todo.id
                todo.projectId
                todo.todo
                (if todo.status == "Done" then
                    Done

                 else
                    Pending
                )
        )


init : Flags -> U.Url -> BN.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { page = getPageFromUrl url
      , navKey = navKey
      , todos = parseFlagForTodo flags.todos
      , projects = flags.projects
      , addTodoWidget = AddTodoInput.init
      , addProjectWidget =
            { projectName = ""
            , projectWidgetState = ShowLink
            }
      }
    , BN.pushUrl navKey (U.toString url)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                B.Internal url ->
                    ( { model | page = getPageFromUrl url }, BN.pushUrl model.navKey (U.toString url) )

                B.External link ->
                    ( model, BN.load link )

        RouteChanged url ->
            update (AddTodoWidget (AddTodoInput.ToggleState (Just AddTodoInput.ShowLink))) { model | page = getPageFromUrl url }

        ToggleTodoState id ->
            let
                toggleState : Todo -> Todo
                toggleState todo =
                    { todo
                        | status =
                            if todo.status == Pending then
                                Done

                            else
                                Pending
                    }

                newTodos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                toggleState todo

                            else
                                todo
                        )
                        model.todos
            in
            ( { model | todos = newTodos }, saveTodos (convertToJs newTodos) )

        AddTodoWidget addTodoWidgetMsg ->
            case addTodoWidgetMsg of
                AddTodoInput.AddTodo todoText ->
                    case model.page of
                        ProjectPage projectId ->
                            ( { model
                                | addTodoWidget = Tuple.first (AddTodoInput.update addTodoWidgetMsg model.addTodoWidget)
                              }
                            , Random.generate
                                (\int ->
                                    AddTodo
                                        (Todo (String.fromInt int) projectId todoText Pending)
                                )
                                (Random.int (List.length model.todos) 1000)
                            )

                        _ ->
                            ( { model
                                | addTodoWidget = Tuple.first (AddTodoInput.update addTodoWidgetMsg model.addTodoWidget)
                              }
                            , Cmd.none
                            )

                _ ->
                    let
                        ( mdl, cmd ) =
                            AddTodoInput.update addTodoWidgetMsg model.addTodoWidget
                    in
                    ( { model
                        | addTodoWidget = mdl
                      }
                    , Cmd.map (\_ -> NoOp) cmd
                    )

        AddTodo todo ->
            let
                newTodos =
                    model.todos ++ [ todo ]
            in
            ( { model
                | todos = newTodos
              }
            , saveTodos (convertToJs newTodos)
            )

        AddProject project ->
            let
                newProjects =
                    model.projects ++ [ project ]

                addProjectWidget =
                    { projectName = ""
                    , projectWidgetState = ShowLink
                    }
            in
            ( { model
                | projects = newProjects
                , addProjectWidget = addProjectWidget
              }
            , saveProjects newProjects
            )

        ToggleProjectWidgetState state ->
            ( { model
                | addProjectWidget =
                    { projectWidgetState = state
                    , projectName = model.addProjectWidget.projectName
                    }
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus "add-project-input")
            )

        GenerateNewProject ->
            ( model
            , Random.generate
                (\int ->
                    AddProject
                        { id = String.fromInt int
                        , name = model.addProjectWidget.projectName
                        }
                )
                (Random.int (List.length model.projects) 10000)
            )

        UpdateProjectNameInWidget str ->
            ( { model
                | addProjectWidget =
                    { projectName = str
                    , projectWidgetState = model.addProjectWidget.projectWidgetState
                    }
              }
            , Cmd.none
            )

        DeleteTodo todoId ->
            ( { model
                | todos = List.filter (\todo -> todo.id /= todoId) model.todos
              }
            , Cmd.none
            )

        ShowDeleteConfirmation todoId ->
            ( model, showDeleteConfirmation todoId )

        ProcessDeleteTodoConfirmation ( shouldDelete, todoId ) ->
            if shouldDelete then
                update (DeleteTodo todoId) model

            else
                ( model, Cmd.none )

        HideInputs ->
            ( { model
                | addProjectWidget =
                    { projectName = ""
                    , projectWidgetState = ShowLink
                    }
                , addTodoWidget =
                    { todoText = ""
                    , state = AddTodoInput.ShowLink
                    }
              }
            , Cmd.none
            )


convertToJs : List Todo -> List TodoJS
convertToJs =
    List.map
        (\todo ->
            { id = todo.id
            , projectId = todo.projectId
            , todo = todo.todo
            , status =
                if todo.status == Done then
                    "Done"

                else
                    "Pending"
            }
        )


getPageFromUrl : U.Url -> Page
getPageFromUrl url =
    let
        pageParser =
            UP.oneOf
                [ UP.map ProjectPage (UP.s "projects" </> UP.string)
                , UP.map HomePage UP.top
                ]

        parsedPage =
            UP.parse pageParser url
    in
    Maybe.withDefault NotFound parsedPage


view : Model -> B.Document Msg
view model =
    case model.page of
        HomePage ->
            renderPage
                { title = "Hello"
                , body =
                    H.div
                        []
                        [ viewHeader False "Todos: "
                        , H.div []
                            [ viewTodos { todos = model.todos, projects = model.projects }
                            ]
                        , H.div
                            [ Attr.class "mt-10"
                            ]
                            [ H.div
                                [ Attr.class "text-xl mb-3"
                                ]
                                [ H.text "Projects:" ]
                            , viewProjects (getProjectsWithTodoCount model)
                            , H.div [ Attr.class "mt-3" ]
                                [ viewAddProject model ]
                            ]
                        ]
                }

        ProjectPage projectId ->
            renderPage
                { title = getProjectName projectId model.projects
                , body =
                    H.div []
                        [ viewHeader True (getProjectName projectId model.projects)
                        , H.div []
                            [ viewTodosForProject (List.filter (\todo -> todo.projectId == projectId) model.todos)
                            ]
                        , H.div [ Attr.class "mt-3" ] [ H.map AddTodoWidget (AddTodoInput.view model.addTodoWidget) ]
                        ]
                }

        NotFound ->
            { title = "Not found"
            , body =
                [ H.div [] [ H.text "404" ]
                ]
            }


viewTodo : List Project -> Todo -> H.Html Msg
viewTodo projects todo =
    H.li
        [ Attr.class "px-1 py-2 flex items-center justify-between"
        ]
        [ H.div
            [ Attr.class "flex items-center"
            ]
            [ H.span
                [ Attr.class
                    "cursor-pointer"
                , Attr.class
                    (if todo.status == Done then
                        "done line-through"

                     else
                        ""
                    )
                , Ev.onClick (ToggleTodoState todo.id)
                ]
                [ H.text todo.todo ]
            , H.a
                [ Attr.class "d-inline-block text-gray-300 text-xs ml-2"
                , Attr.href ("/projects/" ++ todo.projectId)
                ]
                [ H.text (getProjectName todo.projectId projects) ]
            ]
        , H.span
            [ Attr.class "cursor-pointer d-inline-block text-red-300 text-xs"
            , Ev.onClick (ShowDeleteConfirmation todo.id)
            ]
            [ H.text "Del" ]
        ]


viewTodos : { todos : List Todo, projects : List Project } -> H.Html Msg
viewTodos { todos, projects } =
    H.ul [] (List.map (viewTodo projects) todos)


viewTodosForProject : List Todo -> H.Html Msg
viewTodosForProject todos =
    H.ul []
        (List.map
            (\todo ->
                H.li
                    [ Attr.class "px-1 py-2 flex items-center justify-between"
                    ]
                    [ H.div
                        [ Attr.class "flex items-center"
                        ]
                        [ H.span
                            [ Attr.class
                                "cursor-pointer"
                            , Attr.class
                                (if todo.status == Done then
                                    "done line-through"

                                 else
                                    ""
                                )
                            , Ev.onClick (ToggleTodoState todo.id)
                            ]
                            [ H.text todo.todo ]
                        ]
                    , H.a
                        [ Attr.class "cursor-pointer d-inline-block text-red-300 text-xs" ]
                        [ H.text "Del" ]
                    ]
            )
            todos
        )


getProjectName : String -> List Project -> String
getProjectName id projects =
    let
        filtered =
            List.filter (\project -> project.id == id) projects

        head =
            List.head filtered
    in
    case head of
        Nothing ->
            ""

        Just project ->
            project.name


type alias ProjectWithSummary =
    { id : String
    , name : String
    , numberOfTodos : Int
    }


viewProjects : List ProjectWithSummary -> H.Html Msg
viewProjects projects =
    H.ul []
        (List.map
            viewProject
            projects
        )


viewProject : ProjectWithSummary -> H.Html Msg
viewProject project =
    let
        str =
            project.name ++ " (pending todos: " ++ String.fromInt project.numberOfTodos ++ ")"
    in
    H.li []
        [ H.a [ Attr.href ("/projects/" ++ project.id) ] [ H.text str ]
        ]


getProjectsWithTodoCount : Model -> List ProjectWithSummary
getProjectsWithTodoCount model =
    let
        getTodoCount : String -> Int
        getTodoCount projectId =
            List.filter (\todo -> todo.projectId == projectId && todo.status /= Done) model.todos |> List.length

        mkProjectWithSummary : Project -> ProjectWithSummary
        mkProjectWithSummary project =
            { id = project.id, name = project.name, numberOfTodos = getTodoCount project.id }
    in
    List.map mkProjectWithSummary model.projects


port saveTodos :
    List
        { id : String
        , projectId : String
        , todo : String
        , status : String
        }
    -> Cmd msg


port saveProjects : List Project -> Cmd msg


viewAddProject : Model -> H.Html Msg
viewAddProject { addProjectWidget } =
    case addProjectWidget.projectWidgetState of
        ShowLink ->
            H.div
                []
                [ H.span
                    [ Attr.class "a"
                    , Attr.id "add-project-link"
                    , Ev.onClick (ToggleProjectWidgetState ShowInput)
                    ]
                    [ H.text "+ Add project" ]
                ]

        ShowInput ->
            H.div
                [ Attr.class "flex items-center justify-between"
                ]
                [ H.input
                    [ Attr.placeholder "Project Y"
                    , Ev.onInput UpdateProjectNameInWidget
                    , Attr.id "add-project-input"
                    , Ev.on "keyup"
                        (Decoder.map
                            (\code ->
                                if code == 13 then
                                    GenerateNewProject

                                else
                                    NoOp
                            )
                            Ev.keyCode
                        )
                    ]
                    []
                , H.button
                    [ Ev.onClick GenerateNewProject
                    , Attr.id "add-project-btn"
                    , Attr.disabled
                        (if String.trim addProjectWidget.projectName == "" then
                            True

                         else
                            False
                        )
                    ]
                    [ H.text "Add project" ]
                ]


port showDeleteConfirmation : String -> Cmd msg


port processDeleteConfirmation : (( Bool, String ) -> msg) -> Sub msg


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ processDeleteConfirmation ProcessDeleteTodoConfirmation
        , hideInputs (\_ -> HideInputs)
        ]


port hideInputs : (Bool -> msg) -> Sub msg
