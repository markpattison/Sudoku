module Analyse

open Grid

type CellAnalysis =
    | Known of int
    | Possibles of int Set
with
    member _x.NumberPossible =
        match _x with
        | Known _ -> 1
        | Possibles p -> Set.count p

let isUnknown cellAnalysis =
    match cellAnalysis with
    | Known _ -> false
    | _ -> true

let basicAnalysis grid =
    let allPossibles = [ 1 .. grid.MaxValue] |> Set.ofList
    let cells = grid.Cells |> Seq.cast<(Cell<int option>)> |> List.ofSeq
    let othersInRow x y = cells |> List.where (fun cell -> cell.Row = y && cell.Column <> x) |> List.choose (fun cell -> cell.Content) |> Set.ofList
    let othersInColumn x y = cells |> List.where (fun cell -> cell.Column = x && cell.Row <> y) |> List.choose (fun cell -> cell.Content) |> Set.ofList
    let othersInRegion x y = cells |> List.where (fun cell -> cell.Region = Grid.Region x y grid.Size && (cell.Column <> x || cell.Row <> y)) |> List.choose (fun cell -> cell.Content) |> Set.ofList
    let f x y =
        let content = grid.Cells.[x, y].Content
        match content with
        | Some n -> Known n
        | None ->
            Possibles (allPossibles - othersInRow x y - othersInColumn x y - othersInRegion x y)
    New grid.Size f

let cellsWithOnePossible cell =
    match cell.Content with
    | Possibles possibles when Set.count possibles = 1 -> Some ({ Row = cell.Row; Column = cell.Column; Region = cell.Region; Content = Set.minElement possibles })
    | _ -> None

let cellsContainingValue value cell =
    match cell.Content with
    | Possibles possibles when Set.contains value possibles -> true
    | _ -> false

let searchArea cells maxValue areaIdentifier =
    let grouped = Seq.groupBy areaIdentifier cells
    let checkForValue value (_, cellsInArea) =
        match Seq.exists (fun cell -> cell.Content = Known value) cellsInArea with
        | false ->
            let possibleCells = cellsInArea |> Seq.filter (cellsContainingValue value)
            if Seq.length possibleCells = 1 then
                let cell = Seq.exactlyOne possibleCells
                Some ({ Row = cell.Row; Column = cell.Column; Region = cell.Region; Content = value})
             else None
        | true -> None
    let checkAreaForValue value = Seq.choose (checkForValue value) grouped
    [1 .. maxValue] |> Seq.collect checkAreaForValue

let foundCells (grid: Grid<CellAnalysis>) =
    let cells = grid.Cells |> Seq.cast<(Cell<CellAnalysis>)>

    let found1 = Seq.choose cellsWithOnePossible cells |> Set.ofSeq
    let foundByRow = searchArea cells grid.MaxValue (fun c -> c.Row) |> Set.ofSeq
    let foundByColumn = searchArea cells grid.MaxValue (fun c -> c.Column) |> Set.ofSeq
    let foundByRegion = searchArea cells grid.MaxValue (fun c -> c.Region) |> Set.ofSeq

    found1 + foundByRow + foundByColumn + foundByRegion

type SolveResult =
    | Impossible
    | MultipleSolutions
    | Solution of Grid<int option>

let isMultiple result =
    match result with
    | MultipleSolutions -> true
    | _-> false

let isSolution result =
    match result with
    | Solution _ -> true
    | _ -> false

let trialAndError grid analysis =
    let unknownCells = analysis.Cells |> Seq.cast<Cell<CellAnalysis>> |> Seq.filter (fun cell -> cell.Content.NumberPossible > 1)
    let cellToTry = unknownCells |> Seq.minBy (fun cell -> cell.Content.NumberPossible)
    let possibles =
        match cellToTry.Content with
        | Possibles p -> p
        | _ -> failwith "error"
    let update = Grid.Update grid
    let gridsToTry = possibles |> Seq.map (fun p -> { Row = cellToTry.Row ; Column = cellToTry.Column; Region = cellToTry.Region; Content = p } |> Seq.singleton |> update) |> List.ofSeq
    gridsToTry

let anyErrors grid =
    let anyDuplicates cells =
        let knownCells = cells |> Seq.choose (fun cell -> cell.Content)
        let distinct = Seq.distinct knownCells
        not (Seq.length distinct = Seq.length knownCells)
    let cells = grid.Cells |> Seq.cast<Cell<int option>>
    let groupedByRow = Seq.groupBy (fun cell -> cell.Row) cells |> Seq.map snd
    let groupedByColumn = Seq.groupBy (fun cell -> cell.Column) cells |> Seq.map snd
    let groupedByRegion = Seq.groupBy (fun cell -> cell.Region) cells |> Seq.map snd
    let allGroups = groupedByRow |> Seq.append groupedByColumn |> Seq.append groupedByRegion
    Seq.exists (fun cellsInArea -> anyDuplicates cellsInArea) allGroups

let rec Solve grid =
    let numberUnknownCells = Grid.EmptyCells grid
    match numberUnknownCells with
    | 0 ->
        let g = Grid.Print grid
        Solution grid
    | _ ->
        let analysis = basicAnalysis grid
        let anyImpossibleCells = analysis.Cells |> Seq.cast<Cell<CellAnalysis>> |> Seq.exists (fun cell -> cell.Content.NumberPossible = 0)
        match anyImpossibleCells with
        | true -> Impossible
        | false ->
            let found = foundCells analysis
            let numberFound = Seq.length found
            let g = Grid.Print grid
            match numberFound with
            | 0 ->
                let trials = trialAndError grid analysis
                let trialResults = trials |> List.map Solve
                let anyMultiple = trialResults |> List.exists isMultiple
                let solutions = trialResults |> List.filter isSolution
                match (anyMultiple, solutions) with
                | true, _ -> MultipleSolutions
                | false, [] -> Impossible
                | false, [solution] -> solution
                | _ -> MultipleSolutions
            | _ ->
                let updated = Grid.Update grid found
                match anyErrors updated with
                | true -> Impossible
                | false -> Solve updated

