module program

open System
open System.IO
open FParsec

let parse text : (int list) list =
    let tile = digit |>> (sprintf "%c" >> Int32.Parse)
    let row = many1 tile
    let grid = sepEndBy row newline .>> eof

    match run grid text with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let closes (grid: int [,]) (x, y) : (int * int) list =
    let mutable closes = []

    if x - 1 >= 0 then
        closes <- (x - 1, y) :: closes

    if x + 1 < Array2D.length2 grid then
        closes <- (x + 1, y) :: closes

    if y - 1 >= 0 then
        closes <- (x, y - 1) :: closes

    if y + 1 < Array2D.length1 grid then
        closes <- (x, y + 1) :: closes

    closes

let islandSize (grid: int [,]) (x, y) : int =
    let visitedCells = set [ (x, y) ]

    let rec islandSizeRec visitedCells =
        let closes =
            Set.fold (fun s coord -> Set.union s (set (closes grid coord))) Set.empty visitedCells

        let front =
            Set.difference closes visitedCells
            |> Set.filter (fun (x, y) -> Array2D.get grid y x <> 9)

        match front with
        | x when x.IsEmpty -> visitedCells
        | x -> islandSizeRec (Set.union visitedCells x)

    islandSizeRec visitedCells
    |> Set.toList
    |> List.length

let minCoords (grid: int [,]) : (int * int) list =
    grid
    |> Array2D.mapi (fun y x v -> v, closes grid (x, y))
    |> fun grid' ->
        let mutable acc = []

        Array2D.iteri
            (fun y x (v, closes) ->
                if List.forall (fun (x, y) -> v < Array2D.get grid y x) closes then
                    acc <- (x, y) :: acc)
            grid'

        acc


[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input"
    let grid = parse text |> array2D

    let minCoords = minCoords grid

    let answerOne =
        List.fold (fun s (x, y) -> s + (Array2D.get grid y x) + 1) 0 minCoords

    printfn $"Answer for part one is: {answerOne}"

    let answerTwo =
        minCoords
        |> List.map (islandSize grid)
        |> List.sortDescending
        |> List.take 3
        |> List.reduce (*)

    printfn $"Answer for part two is: {answerTwo}"

    0
