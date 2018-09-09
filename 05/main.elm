module Main exposing (Block, Direction(..), Model, Msg(..), Snake, collidingWithWall, createBlock, createBlockRect, init, initSnake, main, moveSnake, size, sizeString, snakeHead, subscriptions, update, updateGameState, view)

import Browser exposing (..)
import Html exposing (..)
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
    , directon : Direction
    , gameOver : Bool
    }


type alias Block =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Block


type Direction
    = Left
    | Right
    | Up
    | Down


size =
    700


sizeString =
    size
        |> String.fromInt


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initSnake Left False
    , Cmd.none
    )


initSnake : Snake
initSnake =
    [ createBlock 24 24
    , createBlock 25 24
    , createBlock 26 24
    ]


createBlock : Int -> Int -> Block
createBlock x y =
    Block x y



-- UPDATE


type Msg
    = None
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            updateGameState model

        None ->
            ( model, Cmd.none )


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
            |> moveSnake


collidingWithWall : ( Model, Cmd msg ) -> ( Model, Cmd msg )
collidingWithWall ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead
    in
    if head.x == 0 || head.x == size || head.y == 0 || head.x == size then
        ( { model | gameOver = True }, cmd )

    else
        ( model, cmd )



-- ( model, cmd )


moveSnake : ( Model, Cmd msg ) -> ( Model, Cmd msg )
moveSnake ( model, cmd ) =
    let
        head =
            model.snake
                |> snakeHead

        newHead =
            { head | x = head.x - 1 }

        newSnake : Snake
        newSnake =
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

        renderStack =
            List.append background snake
    in
    svg
        [ width sizeString, height sizeString, viewBox "0 0 700 700" ]
        renderStack


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
    every 1000 Tick
