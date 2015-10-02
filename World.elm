module World (Model, init, update, view) where

import Cell exposing (..)

import Dict
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Set exposing (filter, foldl, fromList, member, toList, union)
--import Time exposing (fps)
--import Window

numCells = 40

-- MODEL
type alias Model = List Cell.Model

init : Model
init =
  List.concatMap (\x -> (
    List.map (Cell.init False x) [1..numCells]
    )
  ) [1..numCells]

-- UPDATE
type Action = Click Cell.Model Cell.Action | AdvanceGeneration

update : Action -> Model -> Model
update action model =
  case action of
    Click cell cellAction ->
      let updateCell cellModel =
        if cellModel.x == cell.x && cellModel.y == cell.y
          then (Cell.update cellAction cellModel)
          else cellModel
      in
      List.map updateCell model
    AdvanceGeneration ->
      let
        -- Get a list and a set of the live cells in the form of a tuple of
        -- x and y coordinates.
        aliveList =
          List.filter .alive model
            |> List.map (\c -> (c.x, c.y))
        aliveSet =
          Set.fromList aliveList
        -- Get a set of all the neighbors for the live cells.
        neighborSet =
          List.concatMap getNeighbors aliveList |> Set.fromList
        -- Filter out all the currently living cells that will stay alive into
        -- the next generation.
        stayAlive = 
          Set.filter (\n -> 
              ((countAliveNeighbors aliveSet n) == 3 ||
               (countAliveNeighbors aliveSet n) == 2)
              ) aliveSet
        -- Filter all of the neighbors that should be born.
        born =
          Set.filter (\n -> 
              ((countAliveNeighbors aliveSet n) == 3)
              ) neighborSet
        -- Consolidate the live cells into a single set and make a new model.
        newAliveSet =
          Set.union stayAlive born
        initCell x y =
          if Set.member (x, y) newAliveSet then
            Cell.init True x y
          else
            Cell.init False x y
        newModel =      
          List.concatMap (\x -> (
            List.map (initCell x) [1..numCells]
            )
          ) [1..numCells]
      in
      newModel

countAliveNeighbors : Set.Set (Int, Int) -> (Int, Int) -> Int
countAliveNeighbors aliveCells cell =
  getNeighbors cell
    |> Set.fromList
    |> Set.filter (\n -> Set.member n aliveCells)
    |> Set.foldl (\(x, _) -> (+) (x//x)) 0

getNeighbors : (Int, Int) -> List (Int, Int)
getNeighbors (x, y) =
  [ (x-1, y-1)
  , (x  , y-1)
  , (x+1, y-1)
  , (x-1, y)
  , (x+1, y)
  , (x-1, y+1)
  , (x  , y+1)
  , (x+1, y+1)
  ]

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let
    dim = "px" |> (++) (toString ((Cell.size+2)*numCells))
    rowStyle = style [("width", dim)]
  in
  div []
    [ div [rowStyle] (List.map (viewCell address) model)
    , button [onClick address AdvanceGeneration] [text "++ Generation ++"]
    ]

viewCell : Signal.Address Action -> Cell.Model -> Html
viewCell address model =
  Cell.view (Signal.forwardTo address (Click model)) model

