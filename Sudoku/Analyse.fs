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
    let cells = grid.Cells |> Seq.cast<(Cell * int option)> |> List.ofSeq
    let othersInRow x y = cells |> List.where (fun (cell, _) -> cell.Row = y && cell.Column <> x) |> List.choose snd |> Set.ofList
    let othersInColumn x y = cells |> List.where (fun (cell, _) -> cell.Column = x && cell.Row <> y) |> List.choose snd |> Set.ofList
    let othersInRegion x y = cells |> List.where (fun (cell, _) -> cell.Region = Grid.Region x y grid.Size && (cell.Column <> x || cell.Row <> y)) |> List.choose snd |> Set.ofList
    let f x y =
        let (_, content) = grid.Cells.[x, y]
        match content with
        | Some n -> Known n
        | None ->
            Possibles (allPossibles - othersInRow x y - othersInColumn x y - othersInRegion x y)
    New grid.Size f

let cellsWithOnePossible (cell, analysis) =
    match analysis with
    | Possibles possibles when Set.count possibles = 1 -> Some (cell, Set.minElement possibles)
    | _ -> None

let cellsContainingValue value (cell, analysis) =
    match analysis with
    | Possibles possibles when Set.contains value possibles -> true
    | _ -> false

let searchArea cells maxValue areaIdentifier =
    let grouped = Seq.groupBy (fun (cell, _) -> areaIdentifier cell) cells
    let checkForValue value (_, cellsInArea) =
        match Seq.exists (fun (cell, content) -> content = Known value) cellsInArea with
        | false ->
            let possibleCells = cellsInArea |> Seq.filter (cellsContainingValue value) |> Seq.map fst
            if Seq.length possibleCells = 1 then Some (Seq.exactlyOne possibleCells, value) else None
        | true -> None
    let checkAreaForValue value = Seq.choose (checkForValue value) grouped
    [1 .. maxValue] |> Seq.collect checkAreaForValue

let foundCells (grid: Grid<CellAnalysis>) =
    let cells = grid.Cells |> Seq.cast<(Cell * CellAnalysis)>

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
    let unknownCells = analysis.Cells |> Seq.cast<Cell * CellAnalysis> |> Seq.filter (fun (_, content) -> content.NumberPossible > 1)
    let (cellToTry, cellAnalysis) = unknownCells |> Seq.minBy (fun (cell, content) -> content.NumberPossible)
    let possibles =
        match cellAnalysis with
        | Possibles p -> p
        | _ -> failwith "error"
    let update = Grid.Update grid
    let gridsToTry = possibles |> Seq.map (fun p -> (cellToTry, p) |> Seq.singleton |> update) |> List.ofSeq
    gridsToTry

let rec Solve grid =
    let numberUnknownCells = Grid.EmptyCells grid
    match numberUnknownCells with
    | 0 -> Solution grid
    | _ ->
        let analysis = basicAnalysis grid
        let anyImpossibleCells = analysis.Cells |> Seq.cast<Cell * CellAnalysis> |> Seq.exists (fun (_, content) -> content.NumberPossible = 0)
        match anyImpossibleCells with
        | true -> Impossible
        | false ->
            let found = foundCells analysis
            let numberFound = Seq.length found
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
                Solve updated

