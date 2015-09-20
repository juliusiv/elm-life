module Environment (Model, init, update, view) where

import Cell exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (src, style)
--import Time exposing (fps)
--import Window

size = 10

-- MODEL
type alias Model = List Cell.Model

init : Model
init =
  List.map Cell.init [1..(size*size)]

-- UPDATE
type Action = Click Cell.ID Cell.Action | Tick

update : Action -> Model -> Model
update action model =
  --model
  case action of
    Click id cellAction ->
      let updateCell cellModel =
        if cellModel.id == id
          then (Cell.update cellAction cellModel)
          else cellModel
      in
      List.map updateCell model
    Tick ->
      model

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let
    rowStyle = style [("width", "220px")]
  in
  div [rowStyle] (List.map (viewCell address) model)

viewCell : Signal.Address Action -> Cell.Model -> Html
viewCell address model =
  Cell.view (Signal.forwardTo address (Click model.id)) model

