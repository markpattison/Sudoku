module Analyse

open Grid

let isPossiblesNumber cell =
    match cell.Content with
    | Possibles possibles -> Some (cell, Set.count possibles)
    | _ -> None

let isKnownValue cell =
    match cell.Content with
    | Known n -> Some n
    | _ -> None

let notKnown cell =
    match cell.Content with
    | Known _ -> false
    | _ -> true

let isImpossible cell =
    match cell.Content with
    | Possibles possibles when Set.count possibles = 0 -> true
    | _ -> false

let basicAnalysis grid =
    let allPossibles = [ 1 .. grid.MaxValue ] |> Set.ofList
    let ruledOut column row =
        grid.Cells
        |> List.where (fun cell -> cell.Row = row || cell.Column = column || cell.Region = Region column row grid.Size)
        |> List.choose isKnownValue
        |> Set.ofList
    let analyse column row  =
        let content = ContentAt grid column row
        match content with
        | Known n -> Known n
        | Unknown -> Possibles (allPossibles - ruledOut column row)
        | Possibles _ -> failwith "error"
    New grid.Size analyse

let cellsWithOnePossible cell =
    match cell.Content with
    | Possibles possibles when Set.count possibles = 1 -> Some ({ cell with Content = Known (Set.minElement possibles) })
    | _ -> None

let cellsPossiblyContainingValue value cell =
    match cell.Content with
    | Possibles possibles when Set.contains value possibles -> true
    | _ -> false

let searchArea maxValue groups =
    let checkForValue value cellsInArea =
        let possibleCells = cellsInArea |> List.filter (cellsPossiblyContainingValue value)
        if List.length possibleCells = 1 then
            let cell = List.exactlyOne possibleCells
            Some ({ cell with Content = Known value})
        else None
    let checkAreaForValue value = List.choose (checkForValue value) groups
    [1 .. maxValue] |> List.collect checkAreaForValue

let foundCells (grid: Grid) =
    let found1 = List.choose cellsWithOnePossible grid.Cells |> Set.ofList
    let found2 = searchArea grid.MaxValue (GroupedRegions grid) |> Set.ofList

    (found1 + found2) |> List.ofSeq

type SolveResult =
    | Impossible
    | MultipleSolutions
    | Solution of Grid

let isMultiple result =
    match result with
    | MultipleSolutions -> true
    | _-> false

let isSolution result =
    match result with
    | Solution _ -> true
    | _ -> false

let trialAndError grid analysis =
    let unknownCells = analysis.Cells |> List.choose isPossiblesNumber
    let cellToTry = unknownCells |> List.minBy snd |> fst
    let possibles =
        match cellToTry.Content with
        | Possibles p -> p
        | _ -> failwith "error"
    let update = Grid.Update grid
    let gridsToTry = possibles |> Seq.map (fun p -> { cellToTry with Content = Known p } |> List.singleton |> update) |> List.ofSeq
    gridsToTry

let anyErrors grid =
    let anyDuplicates cells =
        let knownCells = cells |> List.choose isKnownValue
        let distinct = List.distinct knownCells
        not (List.length distinct = List.length knownCells)
    List.exists (fun cellsInArea -> anyDuplicates cellsInArea) (GroupedRegions grid)

let rec Solve grid =
    let numberUnknownCells = grid.Cells |> List.filter notKnown |> List.length
    match numberUnknownCells with
    | 0 ->
        match anyErrors grid with
        | true -> Impossible
        | false -> Solution grid
    | _ ->
        let analysis = basicAnalysis grid
        let anyImpossibleCells = analysis.Cells |> List.exists isImpossible
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
                match anyErrors updated with
                | true -> Impossible
                | false -> Solve updated

