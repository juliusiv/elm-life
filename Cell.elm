module Cell (Action, Model, ID, init, update, view) where

import Color exposing (..)
import Debug exposing (log)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = {
  alive : Bool,
  id    : ID
}

type alias ID = Int

init : Int -> Model
init id =
  { alive = False, id = id }

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
        , ("background-color", if model.alive then "blue" else "white")
        ]
  in
  div [cellStyle, onClick address Click] []
