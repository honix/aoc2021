module program

open System
open System.IO

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parseOperation (string : string) = 
    match string.Split [| ' ' |] with
    | [| c; n |] ->
        match Int32.TryParse n with
        | (true, units) ->
            match c with
            | "forward" -> Forward units
            | "down"    -> Down    units
            | "up"      -> Up      units
            | _ -> failwith $"unknown command {c}"
        | _ -> failwith $"unknown int {n}"
    | _ -> failwith $"unknown operation {string}"

let rec step (horizontal, depth) command =
    match command with
    | Forward units -> horizontal + units, depth
    | Down units    -> horizontal        , depth + units
    | Up units      -> horizontal        , depth - units

let rec stepAim (horizontal, depth, aim) command =
    match command with
    | Forward units -> horizontal + units, depth + aim * units, aim
    | Down units    -> horizontal        , depth              , aim + units
    | Up units      -> horizontal        , depth              , aim - units


[<EntryPoint>]
let main _ =
    let commands =
        File.ReadLines @"input"
        |> Seq.map parseOperation

    let ``answer one`` = 
        Seq.fold step (0, 0) commands
        ||> (*)

    let ``answer two`` = 
        Seq.fold stepAim (0, 0, 0) commands
        |> (fun (x, y, _) -> (x, y))
        ||> (*)

    printfn $"Answer for part one is: {``answer one``}"
    printfn $"Answer for part two is: {``answer two``}"

    0
