module Cell (Action, Model, ID, init, update, view) where

import Color exposing (..)
import Debug exposing (log)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- MODEL
type alias Model = {
  alive : Bool,
  id : ID
}

type alias ID = Int

init : Int -> Model
init id =
  { alive = False, id = id }

-- UPDATE
type Action = Click
--type Action = Born | Die

update : Action -> Model -> Model
update action model =
  case action of
    Click -> { model | alive <- (log "Alive" (not model.alive)) }
    --Born -> { model | alive <- (log "Alive" True) }
    --Die  -> { model | alive <- (log "Alive" False) }

-- VIEW
view : Signal.Address Action -> Model -> Html
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
  --square 20 |> outlined (solid black)