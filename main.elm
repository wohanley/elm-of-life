import ListUtil as LU
import Math

type CellState = Bool
type Row = [CellState]
type Grid = [[CellState]]

main : Element
main = renderGrid (generateGrid 30 30)

generateGrid : Int -> Int -> Grid
generateGrid rows cols =
    repeat rows (repeat cols True)

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
    (getNeighbours row column grid)
        |> filter (\cell -> cell)
        |> length
        |> liveOrDie

getNeighbours : Int -> Int -> Int -> Int -> Grid -> [CellState]
getNeighbours maxRow maxColumn row column grid =
    let bound max index = LU.range (Math.max 0 index) (Math.min bound index)
    in map (map LU.get (bound maxRow row)) grid

liveOrDie : Int -> CellState
liveOrDie livingNeighbours =
    case livingNeighbours of
      2 -> True
      3 -> True
      _ -> False