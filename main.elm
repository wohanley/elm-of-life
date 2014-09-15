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
step grid = indexedMap
  (\rowNumber row -> indexedMap
    (\columnNumber _ -> stepCell rowNumber columnNumber grid) row) grid

stepCell : Int -> Int -> Grid -> CellState
stepCell row column grid =
  (getNeighbours row column grid)
    |> filter (\cell -> cell)
    |> length
    |> liveOrDie

getNeighbours : Int -> Int -> Grid -> [CellState]
getNeighbours row column grid = [] -- ???

liveOrDie : Int -> CellState
liveOrDie livingNeighbours =
  case livingNeighbours of
    2 -> True
    3 -> True
    _ -> False

indexedMap : (Int -> a -> b) -> [a] -> [b]
indexedMap f xs = tailIndexedMap f 0 xs

tailIndexedMap : (Int -> a -> b) -> Int -> [a] -> [b]
tailIndexedMap f fromIndex xs =
  case xs of
    [] -> []
    head::tail -> (f fromIndex head) :: (tailIndexedMap f (fromIndex + 1) tail)