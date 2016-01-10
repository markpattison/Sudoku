module Grid

type Cell = { Row: int; Column: int; Region: int}

type Grid<'a> = { Cells: (Cell * 'a) [,]; Size: int; MaxValue: int }

let Region x y n = (y / n) + n * (x / n)

let New n f =
    let nSq = n * n
    let cells = Array2D.init nSq nSq (fun x y -> ({ Row = y; Column = x; Region = Region x y n }, f x y))
    { Cells = cells; Size = n; MaxValue = nSq }

let Update grid updates =
    let newCells = Array2D.copy grid.Cells
    updates |> Seq.iter (fun (cell, content) -> newCells.[cell.Column, cell.Row] <- (cell, Some content))
    { Cells = newCells; Size = grid.Size; MaxValue = grid.MaxValue }

let EmptyCells grid =
    grid.Cells |> Seq.cast<(Cell * int option)> |> Seq.filter (fun (_, content) -> content = None) |> Seq.length