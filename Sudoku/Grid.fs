module Grid

type CellContent =
    | Known of int
    | Possibles of int Set
    | Unknown

type Cell = { Row: int; Column: int; Region: int; Content: CellContent }

type Grid = { Cells: Cell[,]; Size: int; MaxValue: int }

let Region x y n = (y / n) + n * (x / n)

let New n f =
    let nSq = n * n
    let cells = Array2D.init nSq nSq (fun x y -> { Row = y; Column = x; Region = Region x y n; Content = f x y })
    { Cells = cells; Size = n; MaxValue = nSq }

let Update grid updates =
    let newCells = Array2D.copy grid.Cells
    updates |> Seq.iter (fun cell -> newCells.[cell.Column, cell.Row] <- cell)
    { Cells = newCells; Size = grid.Size; MaxValue = grid.MaxValue }

let EmptyCells grid =
    grid.Cells |> Seq.cast<(Cell)> |> Seq.filter (fun cell -> cell.Content = Unknown) |> Seq.length

let cellContentToString (content: CellContent) =
    match content with
    | Known 10 -> "0"
    | Known 11 -> "A"
    | Known 12 -> "B"
    | Known 13 -> "C"
    | Known 14 -> "D"
    | Known 15 -> "E"
    | Known 16 -> "F"
    | Known n -> sprintf "%i" n
    | Unknown | Possibles _ -> "-"

let Print grid =
    let gridContents =
        seq {
            for y in 0 .. (grid.Size * grid.Size - 1) do
                for x in 0 .. (grid.Size * grid.Size - 1) do
                    let content = grid.Cells.[x, y].Content
                    yield (cellContentToString content)
                yield "\n"
        }
    String.concat "" gridContents

