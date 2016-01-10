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

let cellContentToString (content: int option) =
    match content with
    | Some 10 -> "0"
    | Some 11 -> "A"
    | Some 12 -> "B"
    | Some 13 -> "C"
    | Some 14 -> "D"
    | Some 15 -> "E"
    | Some 16 -> "F"
    | Some n -> sprintf "%i" n
    | None -> "-"

let Print grid =
    let gridContents =
        seq {
            for y in 0 .. (grid.Size * grid.Size - 1) do
                for x in 0 .. (grid.Size * grid.Size - 1) do
                    let (_, content) = grid.Cells.[x, y]
                    yield (cellContentToString content)
                yield "\n"
        }
    String.concat "" gridContents

