module Main exposing (..)

import Browser as B
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Random as R


main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { gameState : GameState
    , score : Int
    , currentStep : Int
    , currentImageType : ImageType
    , currentImage : String
    , loadingImage : Bool
    }


type GameState
    = Ongoing
    | Done
    | NotStarted


type Msg
    = StartGame
    | GetImage
    | ShowImage ImageDetails
    | SelectAnswer ImageType
    | ShowResult
    | RestartGame


type ImageType
    = Real
    | Fake


type alias ImageDetails =
    { type_ : ImageType
    , src : ImageUrl
    }


type alias ImageUrl =
    String


init =
    let
        initModel =
            Model NotStarted 0 0 Fake "" False
    in
    \() -> ( initModel, Cmd.none )


update msg model =
    case msg of
        StartGame ->
            let
                f =
                    Tuple.first (init ())
            in
            update GetImage { f | gameState = Ongoing }

        GetImage ->
            let
                generator =
                    R.int 1 1000
            in
            ( { model | loadingImage = True }
            , R.generate
                (\int ->
                    ShowImage
                        { type_ = Fake
                        , src = "https://thispersondoesnotexist.com/image?" ++ String.fromInt int
                        }
                )
                generator
            )

        ShowImage details ->
            ( { model
                | currentStep = model.currentStep + 1
                , currentImageType = details.type_
                , currentImage = details.src
                , loadingImage = False
              }
            , Cmd.none
            )

        SelectAnswer answer ->
            let
                updateScore model_ answer_ =
                    if model.currentImageType == answer_ then
                        { model_ | score = model_.score + 1 }

                    else
                        model_
            in
            if model.currentStep /= 5 then
                update GetImage (updateScore model answer)

            else
                update ShowResult (updateScore model answer)

        ShowResult ->
            ( { model | gameState = Done }, Cmd.none )

        RestartGame ->
            init ()


view : Model -> H.Html Msg
view model =
    H.div [ Attr.class "w-2/3 mt-0 mb-0 ml-auto mr-auto h-screen flex items-center justify-center" ]
        [ case model.gameState of
            NotStarted ->
                H.div []
                    [ H.a
                        [ Ev.onClick StartGame
                        , Attr.class "cursor-pointer"
                        ]
                        [ H.text "Start game" ]
                    ]

            Ongoing ->
                if model.loadingImage then
                    H.div [] [ H.text "Loading..." ]

                else
                    H.div []
                        [ H.img
                            [ Attr.src model.currentImage
                            , Attr.width 500
                            , Attr.height 500
                            , Attr.class "border"
                            ]
                            []
                        , H.div
                            [ Attr.class "flex items-center justify-around"
                            ]
                            [ H.a [ Ev.onClick (SelectAnswer Fake) ] [ H.text "Fake" ]
                            , H.a [ Ev.onClick (SelectAnswer Real) ] [ H.text "Real" ]
                            ]
                        ]

            Done ->
                H.div []
                    [ H.div [] [ H.text ("Score: " ++ String.fromInt model.score) ]
                    , H.div [ Ev.onClick RestartGame ] [ H.text "Game over. Click to restart" ]
                    ]
        ]
