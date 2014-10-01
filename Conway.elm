module Conway where

import ListUtil as LU
import Math
import Mouse

type CellState = Bool
type Row = [CellState]
type Grid = [[CellState]]

-- Dealing with randomness purely is disgusting
port initialBoard: [[Bool]] -- aka Grid

cellSize = 10

main : Signal Element
main = handleSignals <~ gameState ~ click

handleSignals : Grid -> (Int, Int) -> Element
handleSignals grid click = renderGrid (handleClick grid click)

gameState : Signal Grid
gameState = foldp (\_ -> step) initialBoard (every second)

-- Display

renderGrid : Grid -> Element
renderGrid = (flow down) . (map renderRow)

renderRow : Row -> Element
renderRow = (flow right) . (map renderCell)

renderCell : CellState -> Element
renderCell cell = color (cellColor cell) (spacer cellSize cellSize)

cellColor : CellState -> Color
cellColor alive = if alive then black else white


-- Logic

step : Grid -> Grid
step grid = LU.indexedMap
  (\rowNumber row -> LU.indexedMap
    (\columnNumber _ -> stepCell rowNumber columnNumber grid) row) grid

stepCell : Int -> Int -> Grid -> CellState
stepCell row column grid =
    let cell = LU.get row grid |> LU.get column
    in (getNeighbours row column grid)
        |> filter (\cell -> cell)
        |> length
        |> liveOrDie cell

getNeighbours : Int -> Int -> Grid -> [CellState]
getNeighbours row column grid =
    let rowCount = length grid
        colCount = length (LU.get 0 grid)
    in
        grid
            |> LU.getAll (bound (rowCount - 1) row)
            |> zipWith (<|)
                   [(LU.getAll (bound (colCount - 1) column)),
                    (LU.getAll (outerBound (colCount - 1) column)),
                    (LU.getAll (bound (colCount - 1) column))]
            |> LU.flatten

bound : Int -> Int -> [Int]
bound max index =
    let lower = Math.max 0 (index - 1)
        upper = Math.min max (index + 1)
    in
        LU.range lower (upper + 1)

outerBound : Int -> Int -> [Int]
outerBound max index = bound max index |> LU.remove index

liveOrDie : CellState -> Int -> CellState
liveOrDie alive livingNeighbours =
    case alive of
      True -> case livingNeighbours of
                2 -> True
                3 -> True
                _ -> False
      False -> case livingNeighbours of
                 3 -> True
                 _ -> False


-- Interaction

click : Signal (Int, Int)
click = sampleOn Mouse.clicks Mouse.position

handleClick : Grid -> (Int, Int) -> Grid
handleClick grid click =
    let gridPosition = getGridPosition cellSize click
    in setCell (toggleCellState (getCell gridPosition grid)) gridPosition grid

getGridPosition : Int -> (Int, Int) -> (Int, Int)
getGridPosition size (x, y) = (x `div` size, y `div` size)

getCell : (Int, Int) -> Grid -> CellState
getCell (column, row) grid = LU.get column (LU.get row grid)

setCell : CellState -> (Int, Int) -> Grid -> Grid
setCell cell (column, row) grid = LU.set2d cell row column grid

toggleCellState : CellState -> CellState
toggleCellState alive = not alive