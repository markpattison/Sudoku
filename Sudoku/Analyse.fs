﻿module Analyse

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
    let allPossibles = [ 1 .. grid.MaxValue] |> Set.ofList
    let cells = grid.Cells
    let othersInRow column row = cells |> List.where (fun cell -> cell.Row = row && cell.Column <> column) |> List.choose isKnownValue |> Set.ofList
    let othersInColumn column row = cells |> List.where (fun cell -> cell.Column = column && cell.Row <> row) |> List.choose isKnownValue |> Set.ofList
    let othersInRegion column row = cells |> List.where (fun cell -> cell.Region = Grid.Region column row grid.Size && (cell.Column <> column || cell.Row <> row)) |> List.choose isKnownValue |> Set.ofList
    let f column row  =
        let content = ContentAt grid column row
        match content with
        | Known n -> Known n
        | Unknown -> Possibles (allPossibles - othersInRow column row - othersInColumn column row - othersInRegion column row)
        | Possibles _ -> failwith "error"
    New grid.Size f

let cellsWithOnePossible cell =
    match cell.Content with
    | Possibles possibles when Set.count possibles = 1 -> Some ({ cell with Content = Known (Set.minElement possibles) })
    | _ -> None

let cellsPossiblyContainingValue value cell =
    match cell.Content with
    | Possibles possibles when Set.contains value possibles -> true
    | _ -> false

let searchArea cells maxValue areaIdentifier =
    let grouped = List.groupBy areaIdentifier cells
    let checkForValue value (_, cellsInArea) =
        let possibleCells = cellsInArea |> List.filter (cellsPossiblyContainingValue value)
        if List.length possibleCells = 1 then
            let cell = List.exactlyOne possibleCells
            Some ({ cell with Content = Known value})
        else None
    let checkAreaForValue value = List.choose (checkForValue value) grouped
    [1 .. maxValue] |> List.collect checkAreaForValue

let foundCells (grid: Grid) =
    let cells = grid.Cells

    let found1 = List.choose cellsWithOnePossible cells |> Set.ofList
    let foundByRow = searchArea cells grid.MaxValue (fun c -> c.Row) |> Set.ofList
    let foundByColumn = searchArea cells grid.MaxValue (fun c -> c.Column) |> Set.ofList
    let foundByRegion = searchArea cells grid.MaxValue (fun c -> c.Region) |> Set.ofList

    (found1 + foundByRow + foundByColumn + foundByRegion) |> List.ofSeq

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
    let cells = grid.Cells
    let groupedByRow = List.groupBy (fun cell -> cell.Row) cells |> List.map snd
    let groupedByColumn = List.groupBy (fun cell -> cell.Column) cells |> List.map snd
    let groupedByRegion = List.groupBy (fun cell -> cell.Region) cells |> List.map snd
    let allGroups = groupedByRow |> List.append groupedByColumn |> List.append groupedByRegion
    List.exists (fun cellsInArea -> anyDuplicates cellsInArea) allGroups

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

