module Main exposing (..)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snake : List Block
    }


type alias Block =
    { x : Int
    , y : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model initSnake
    , Cmd.none
    )


initSnake : List Block
initSnake =
    [ (createBlock 24 24)
    , (createBlock 25 24)
    , (createBlock 26 24)
    ]


createBlock : Int -> Int -> Block
createBlock x y =
    Block x y



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        background =
            rect [ x "10", y "10", width "700", height "700", fill "#7FB13B" ] []
                |> List.singleton

        snake =
            List.map createBodyRect model.snake

        renderStack =
            List.append background snake
    in
        svg
            [ width "700", height "700", viewBox "0 0 700 700" ]
            renderStack


createBodyRect : Block -> Svg msg
createBodyRect block =
    let
        newX =
            block.x
                * 10
                |> toString

        newY =
            block.y
                * 10
                |> toString
    in
        rect [ x newX, y newY, width "10", height "10", fill "#F1F1F1" ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
