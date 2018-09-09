import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  {
  }


init : (Model, Cmd Msg)
init =
  ( Model 
  , Cmd.none
  )



-- UPDATE


type Msg
  = None


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    None ->
      (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    svg
      [ width "700", height "700", viewBox "0 0 700 700" ]
      [ rect [ x "10", y "10", width "700", height "700" ] [] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
