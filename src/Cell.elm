module Cell exposing (Model, Msg(..), init, update, view, size)

import Styles exposing (cell)

import Color exposing (..)
import Debug exposing (log)
import Html exposing (..)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

size = 15

-- MODEL
type alias Model = {
  alive : Bool,
  x     : Int,
  y     : Int
}

init : Bool -> Int -> Int -> Model
init alive x y =
  { alive = alive, x = x, y = y }

-- UPDATE
type Msg = Click

update : Msg -> Model -> Model
update msg model =
  case msg of
    Click -> { model | alive = (log "Alive" (not model.alive)) }

-- VIEW
view : Model -> Html Msg
view model =
  div [(Styles.cell size model.alive), onClick Click] []
