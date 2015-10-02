module Cell (Action, Model, init, update, view) where

import Color exposing (..)
import Debug exposing (log)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

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
type Action = Click

update : Action -> Model -> Model
update action model =
  case action of
    Click -> { model | alive <- (log "Alive" (not model.alive)) }

-- VIEW
view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    cellStyle = 
      style
        [ ("width", "20px")
        , ("height", "20px")
        , ("border", "1px solid black")
        , ("float", "left")
        , ("background-color", if model.alive then "#60B5CC" else "white")
        ]
  in
  div [cellStyle, onClick address Click] []
