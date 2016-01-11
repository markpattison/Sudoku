module Grid

type CellContent =
    | Known of int
    | Possibles of int Set
    | Unknown

type Cell = { Row: int; Column: int; Region: int; Content: CellContent }

type Grid = { Cells: Cell list; Size: int; MaxValue: int; Values: int Set }

let ContentAt grid column row =
    List.find (fun cell -> cell.Column = column && cell.Row = row) grid.Cells |> (fun cell -> cell.Content)

let Region column row n = (row / n) + n * (column / n)

let GroupedRegions grid =
    (List.groupBy (fun c -> c.Row) grid.Cells)
    |> List.append (List.groupBy (fun c -> c.Column) grid.Cells)
    |> List.append (List.groupBy (fun c -> c.Region) grid.Cells)
    |> List.map snd

let New n f =
    let nSq = n * n
    let cells = seq {
        for column in 0 .. (nSq - 1) do
            for row in 0 .. (nSq - 1) do
                yield { Row = row; Column = column; Region = Region column row n; Content = f column row }} |> List.ofSeq
    { Cells = cells; Size = n; MaxValue = nSq; Values = [ 1 .. nSq ] |> Set.ofList }

let Update (grid: Grid) updates =
    let f column row =
        let matchingUpdates = List.tryFind (fun cell -> cell.Row = row && cell.Column = column) updates
        match (matchingUpdates, ContentAt grid column row) with
        | (None, unchanged) -> unchanged
        | (Some update, Unknown) | (Some update, Possibles _) -> update.Content
        | _ -> failwith "error"
    New grid.Size f

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
            for row in 0 .. (grid.Size * grid.Size - 1) do
                for column in 0 .. (grid.Size * grid.Size - 1) do
                    let content = ContentAt grid row column
                    yield (cellContentToString content)
                yield "\n"
        }
    String.concat "" gridContents

