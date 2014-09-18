module Conway where

import ListUtil as LU
import Math

type CellState = Bool
type Row = [CellState]
type Grid = [[CellState]]

-- Dealing with randomness purely is disgusting
port initialBoard: [[Bool]] -- aka Grid

main : Signal Element
main = renderGrid <~ gameState

gameState : Signal Grid
gameState = foldp (\_ -> step) initialBoard (every second)

-- Display

renderGrid : Grid -> Element
renderGrid = (flow down) . (map renderRow)

renderRow : Row -> Element
renderRow = (flow right) . (map renderCell)

cellSize = 10
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