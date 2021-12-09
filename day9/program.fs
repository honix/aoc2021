module program

open System
open System.IO
open FParsec

let parse text =
    let tile = digit |>> (sprintf "%c" >> Int32.Parse)
    let row = many1 tile
    let grid = sepEndBy row newline .>> eof

    match run grid text with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input"
    let grid = parse text |> array2D

    let n =
        Array2D.mapi
            (fun y x v ->
                let mutable closes = []

                if x - 1 >= 0 then
                    closes <- grid.[y, x - 1] :: closes

                if x + 1 < Array2D.length2 grid then
                    closes <- grid.[y, x + 1] :: closes

                if y - 1 >= 0 then
                    closes <- grid.[y - 1, x] :: closes

                if y + 1 < Array2D.length1 grid then
                    closes <- grid.[y + 1, x] :: closes

                List.forall ((<) v) closes, v)
            grid
        |> (fun grid ->
            let mutable n = 0
            Array2D.iter (fun (b, v) -> if b then n <- n + v + 1) grid
            n)

    printfn $"%A{n}"

    0
