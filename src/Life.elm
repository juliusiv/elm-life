module Life exposing (Model, init, update, view, subscriptions)

import Styles

import Array exposing (Array)
import Array2D exposing (Array2D)
import Dict
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attributes exposing (src, style)
import Html.Events exposing (onClick, onInput, onMouseOver)
import Mouse
import Set exposing (Set)
import String exposing (toInt)
import Time exposing (Time, millisecond)

numCells = 35
cellSize = 15
minSpeed = 1000.0
maxSpeed = 100.0

-- MODEL
type alias Model = 
  { cells : Array2D Bool
  , isRunning : Bool
  , isMouseDown : Bool
  , lastUpdate : Float
  , msBetweenUpdates : Float
  }

init : (Model, Cmd Msg)
init =
  ( { cells = Array2D.repeat numCells numCells False
    , isRunning = False
    , isMouseDown = False
    , lastUpdate = 0.0
    , msBetweenUpdates = minSpeed
    }, Cmd.none)


-- UPDATE
type Msg =
  Toggle Int Int
  | Tick Time
  | ChangeSpeed String
  | AdvanceGeneration
  | Start
  | MouseDown Mouse.Position
  | MouseUp Mouse.Position
  | Pause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      if not model.isRunning then
        (model, Cmd.none)
      else if time >= (model.lastUpdate + model.msBetweenUpdates) then
        update AdvanceGeneration {model | lastUpdate = time}
      else
        (model, Cmd.none)

    ChangeSpeed msString ->
      let
        newSpeed = String.toFloat msString |> Result.withDefault minSpeed
      in
        ({model | msBetweenUpdates = newSpeed}, Cmd.none)

    Toggle x y ->
      let
        maybCellState = Array2D.get x y model.cells
      in
        case maybCellState of
          Just cellState ->
            let
              newCells = Array2D.set x y (xor cellState model.isMouseDown) model.cells
            in
              ({model | cells = newCells}, Cmd.none)
          Nothing ->
            -- Something messed up if we're here.
            (model, Cmd.none)

    AdvanceGeneration ->
      ({model | cells = (calculateNewGeneration model.cells)}, Cmd.none)

    Start -> 
      ({model | isRunning = True}, Cmd.none)

    MouseDown _ ->
      ({model | isMouseDown = True}, Cmd.none)

    MouseUp _ ->
      ({model | isMouseDown = False}, Cmd.none)

    Pause ->
      ({model | isRunning = False}, Cmd.none)


flattenArray2D : Array2D a -> List a
flattenArray2D array2D =
  List.concatMap (Array.toList) (Array.toList array2D.data)


calculateNewGeneration : Array2D Bool -> Array2D Bool
calculateNewGeneration cells =
  let
    -- Get a set of the live cells in the form of a tuple of x and y coordinates.
    aliveSet =
      Array2D.indexedMap (\x y cell -> (x, y, cell)) cells
        |> flattenArray2D
        |> List.filter (\(_, _, isAlive) -> isAlive)
        |> List.map (\(x, y, _) -> (x, y))
        |> Set.fromList

    -- Get a set of all the neighbors for the live cells.
    neighborPositions =
      Set.foldl (\(x, y) -> Set.union (getNeighborPositions x y)) Set.empty aliveSet

    -- Filter out all the currently living cells that will stay alive into
    -- the next generation.
    stayAlive = 
      Set.filter (\n -> shouldStayAlive aliveSet n) aliveSet

    -- Filter all of the neighbors that should be born.
    born =
      Set.filter (\n -> shouldBeBorn aliveSet n) neighborPositions

    -- Consolidate the live cells into a single set and make a new model.
    newAliveSet =
      Set.union stayAlive born
  in
    Array2D.indexedMap (\x y cell -> (Set.member (x, y) newAliveSet)) cells


shouldBeBorn : Set (Int, Int) -> (Int, Int) -> Bool
shouldBeBorn aliveCells neighbor =
  (countAliveNeighbors aliveCells neighbor) == 3


shouldStayAlive : Set (Int, Int) -> (Int, Int) -> Bool
shouldStayAlive aliveCells neighbor =
  let
    aliveNeighbors = countAliveNeighbors aliveCells neighbor
  in
    aliveNeighbors == 3 || aliveNeighbors == 2


countAliveNeighbors : Set (Int, Int) -> (Int, Int) -> Int
countAliveNeighbors aliveCells (x, y) =
  getNeighborPositions x y
    |> Set.filter (\n -> Set.member n aliveCells)
    |> Set.foldl (\(x, _) -> (+) (x//x)) 0


getNeighborPositions : Int -> Int -> Set (Int, Int)
getNeighborPositions x y =
  Set.fromList
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
  Sub.batch
    [ Time.every millisecond Tick
    , Mouse.downs MouseDown
    , Mouse.ups MouseUp
    ]


-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ (viewGrid model.cells)
    , div [style [("class", "row"), ("id", "controls")]]
        [ button [onClick (AdvanceGeneration)] [text "Advance Generation"]
        , button [onClick Start] [text "Run Life"]
        , button [onClick Pause] [text "Pause Life"]
        , input
            [ Attributes.type' "range"
            , Attributes.min <| toString maxSpeed
            , Attributes.max <| toString minSpeed
            , Attributes.value <| toString model.msBetweenUpdates
            , onInput ChangeSpeed
            ] []
        ]
    ]


viewGrid : Array2D Bool -> Html Msg
viewGrid cells =
  let
    dim = "px" |> (++) (toString ((cellSize + 2)*numCells))
    rowStyle = style [ ("width", dim)
                     , ("clear", "both")
                     , ("display", "inline-block")
                     , ("border", "1px solid black")
                     ]
    indexedCells = Array2D.indexedMap (\x y cell -> (x, y, cell)) cells
  in
    div [rowStyle] ((Array.map viewRow indexedCells.data) |> Array.toList)


viewRow : Array (Int, Int, Bool) -> Html Msg
viewRow row =
  div [] ((Array.map (\(x, y, cell) -> viewCell x y cell) row) |> Array.toList)


viewCell : Int -> Int -> Bool -> Html Msg
viewCell x y isAlive =
  div [(Styles.cell cellSize isAlive), onMouseOver (Toggle x y), onClick (Toggle x y)] []
