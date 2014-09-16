import ListUtil as LU
import Math
import Random

type CellState = Bool
type Row = [CellState]
type Grid = [[CellState]]

main : Signal Element
main = renderGrid <~ gameState

gameState : Signal Grid
gameState = foldp (\_ -> \gameState -> step gameState) (generateGrid 30 30) (every second)

generateGrid : Int -> Int -> Grid
generateGrid rows cols =
    let randomBool = if Random.float < 0.5 then False else True
    in repeat rows (repeat cols )

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
    let cell = LU.get column (LU.get row grid)
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
            |> LU.getAll (bound rowCount row)
            |> map (LU.getAll (bound colCount column))
            |> LU.flatten

bound : Int -> Int -> [Int]
bound max index =
    let lower = Math.max 0 (index - 1)
        upper = Math.min max (index + 2)
    in
        LU.range lower upper

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