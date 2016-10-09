module World exposing (Model, init, update, view, subscriptions)

import Cell exposing (..)
import Styles exposing (buttonGreen)

import Dict
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Set exposing (filter, foldl, fromList, member, toList, union)
import Time exposing (Time, second)

numCells = 35

-- MODEL
type alias Model = 
  { cells : List Cell.Model
  , isRunning : Bool
  }

init : (Model, Cmd Msg)
init =
  (Model initCells False, Cmd.none)

initCells : List Cell.Model
initCells =
  List.concatMap (\x -> (
    List.map (Cell.init False x) [1..numCells]
    )
  ) [1..numCells]


-- UPDATE
type Msg =
  Toggle Cell.Model Cell.Msg
  | AdvanceGeneration Time
  | Start

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle cell cellAction ->
      let updateCell cellModel =
        if cellModel.x == cell.x && cellModel.y == cell.y
          then (Cell.update cellAction cellModel)
          else cellModel
      in
      ({model | cells = (List.map updateCell model.cells)}, Cmd.none)
    AdvanceGeneration _ ->
      let
        -- Get a list and a set of the live cells in the form of a tuple of
        -- x and y coordinates.
        aliveList =
          List.filter .alive model.cells
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
        if model.isRunning then
          ({model | cells = newModel}, Cmd.none)
        else
          (model, Cmd.none)
    Start -> 
      ({model | isRunning = True}, Cmd.none)


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

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second AdvanceGeneration

-- VIEW
view : Model -> Html Msg
view model =
  let
    dim = "px" |> (++) (toString ((Cell.size+2)*numCells))
    rowStyle = style [("width", dim)]
  in
  div []
    [ div [rowStyle] (List.map (viewCell) model.cells)
    , button [onClick (AdvanceGeneration 0.0)] [text "Advance Generation"]
    , button [onClick Start] [text "Run Life"]
    ]


viewCell : Cell.Model -> Html Msg
viewCell cell =
  div [(Styles.cell size cell.alive), onClick (Toggle cell Cell.Click)] []
