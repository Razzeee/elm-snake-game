module Main exposing (Block, Direction(..), Model, Msg(..), Snake, collidingWithWall, createBlock, createBlockRect, init, initSnake, main, moveSnake, size, sizeString, snakeHead, subscriptions, update, updateGameState, view)

import Browser exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snake : Snake
    , direction : Direction
    , gameOver : Bool
    , cherry : Maybe Cherry
    , points : Int
    , foundCherry : Bool
    }


type alias Block =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Block


type alias Cherry =
    { location : Block
    , points : Int
    }


type Direction
    = Left
    | Right
    | Up
    | Down


gridSize =
    70


size =
    gridSize * 10


sizeString =
    size
        |> String.fromInt


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initSnake Left False (Just initCherry) 0 False
    , Cmd.none
    )


initSnake : Snake
initSnake =
    [ createBlock 24 24
    , createBlock 25 24
    , createBlock 26 24
    , createBlock 27 24
    , createBlock 28 24
    , createBlock 29 24
    , createBlock 30 24
    , createBlock 31 24
    ]


initCherry : Cherry
initCherry =
    { location = Block 15 15
    , points = 15
    }


createBlock : Int -> Int -> Block
createBlock x y =
    Block x y



-- UPDATE


type Msg
    = None
    | Tick Time.Posix
    | ButtonUp
    | ButtonDown
    | ButtonLeft
    | ButtonRight



-- | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            updateGameState model

        ButtonUp ->
            changeDirection Up Down model

        ButtonDown ->
            changeDirection Down Up model

        ButtonLeft ->
            changeDirection Left Right model

        ButtonRight ->
            changeDirection Right Left model

        None ->
            ( model, Cmd.none )


changeDirection : Direction -> Direction -> Model -> ( Model, Cmd Msg )
changeDirection newDirection ignoredDirection model =
    if model.direction == newDirection || model.direction == ignoredDirection then
        ( model, Cmd.none )

    else
        ( { model | direction = newDirection }, Cmd.none )


snakeHead : Snake -> Block
snakeHead snake =
    List.head snake
        |> Maybe.withDefault { x = 0, y = 0 }


updateGameState : Model -> ( Model, Cmd msg )
updateGameState model =
    if model.gameOver then
        ( model
        , Cmd.none
        )

    else
        ( model
        , Cmd.none
        )
            |> collidingWithWall
            |> collidingWithSelf
            |> collidingWithCherry
            |> moveSnake


collidingWithCherry : ( Model, Cmd msg ) -> ( Model, Cmd msg )
collidingWithCherry ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead
    in
    case model.cherry of
        Nothing ->
            ( model, cmd )

        Just cherry ->
            if cherry.location.x == head.x && cherry.location.y == head.y then
                ( { model | points = cherry.points, cherry = Nothing, foundCherry = True }, cmd )

            else
                ( model, cmd )


collidingWithSelf : ( Model, Cmd msg ) -> ( Model, Cmd msg )
collidingWithSelf ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead

        maybeTail =
            List.tail model.snake
    in
    case maybeTail of
        Nothing ->
            ( model, cmd )

        Just tail ->
            if List.member head tail then
                ( { model | gameOver = True }, cmd )

            else
                ( model, cmd )


collidingWithWall : ( Model, Cmd msg ) -> ( Model, Cmd msg )
collidingWithWall ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead
    in
    if head.x > 0 && head.x < gridSize - 1 && head.y > 0 && head.x < gridSize - 1 then
        ( model, cmd )

    else
        ( { model | gameOver = True }, cmd )



-- ( model, cmd )


moveSnake : ( Model, Cmd msg ) -> ( Model, Cmd msg )
moveSnake ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead

        newHead =
            case model.direction of
                Up ->
                    { head | y = head.y - 1 }

                Down ->
                    { head | y = head.y + 1 }

                Left ->
                    { head | x = head.x - 1 }

                Right ->
                    { head | x = head.x + 1 }

        newSnake : Snake
        newSnake =
            case model.foundCherry of
                True ->
                    newHead
                        :: model.snake

                False ->
                    newHead
                        :: model.snake
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
    in
    if model.gameOver then
        ( model
        , Cmd.none
        )

    else
        case model.foundCherry of
            True ->
                ( { model | snake = newSnake, foundCherry = False }, cmd )

            False ->
                ( { model | snake = newSnake }, cmd )



-- VIEW


view : Model -> Html Msg
view model =
    let
        background =
            rect [ x "0", y "0", width sizeString, height sizeString, fill "#7FB13B" ] []
                |> List.singleton

        snake =
            List.map createBlockRect model.snake

        cherry list =
            case model.cherry of
                Nothing ->
                    list

                Just value ->
                    createBlockRect value.location :: list

        renderStack =
            snake
                |> cherry
                |> List.append background
    in
    div []
        [ svg
            [ width sizeString, height sizeString, viewBox "0 0 700 700" ]
            renderStack
        , div []
            [ button [ onClick ButtonUp ] [ Html.text "Up" ]
            , button [ onClick ButtonLeft ] [ Html.text "Left" ]
            , button [ onClick ButtonRight ] [ Html.text "Right" ]
            , button [ onClick ButtonDown ] [ Html.text "Down" ]
            ]
        , div []
            [ case model.gameOver of
                True ->
                    Html.text "Game Over"

                False ->
                    Html.text "Running"
            ]
        , div []
            [ model.points
                |> String.fromInt
                |> String.append "Points: "
                |> Html.text
            ]
        ]


createBlockRect : Block -> Svg msg
createBlockRect block =
    let
        newX =
            block.x
                * 10
                |> String.fromInt

        newY =
            block.y
                * 10
                |> String.fromInt
    in
    rect [ x newX, y newY, width "10", height "10", fill "#F1F1F1" ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every 500 Tick
        ]
