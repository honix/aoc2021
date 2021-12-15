module program

open System
open System.IO
open FParsec

let parse text =
    let cell = digit |>> (sprintf "%c" >> Int32.Parse)
    let line = many1 cell
    let grid = sepEndBy line newline

    match run grid text with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let isCoordsInBounds grid row col =
    List.reduce (&&)
        [
            0 <= row
            0 <= col
            Array2D.length1 grid > row
            Array2D.length2 grid > col
        ]

let iterateDiagBack grid =
    seq {
        let rowMax = Array2D.length1 grid - 1
        let colMax = Array2D.length2 grid - 1
        for diag in rowMax .. -1 .. 0 do
            let mutable row = diag
            let mutable col = colMax
            while isCoordsInBounds grid row col do
                yield row, col
                row <- row + 1
                col <- col - 1
        for diag in colMax - 1 .. -1 .. 0 do
            let mutable row = 0
            let mutable col = diag
            while isCoordsInBounds grid row col do
                yield row, col
                row <- row + 1
                col <- col - 1
    }

let iterateDiagBack2 grid =
    Seq.skip 1 <| iterateDiagBack grid

let closes grid row col =
    seq {
        for srow, scol in [ -1,0 ; 1,0 ; 0,-1 ; 0,1 ] do
            if isCoordsInBounds grid (row + srow) (col + scol) then
                yield row + srow, col + scol
    }

let closeMin (costGrid : int[,]) row col =
    Seq.map
        (fun (row, col) -> Array2D.get costGrid row col)
        (closes costGrid row col)
    |> Seq.min

let costGrid grid =
    let costGrid =
        Array2D.create
            (Array2D.length1 grid)
            (Array2D.length2 grid)
            Int32.MaxValue

    let max = Array2D.length1 grid - 1

    costGrid.[max, max] <- 0

    for row, col in iterateDiagBack2 costGrid do
        let closeMin = closeMin costGrid row col
        costGrid.[row, col] <- closeMin + grid.[row, col]

    costGrid

[<EntryPoint>]
let main _ =
    let grid =
        File.ReadAllText @"input"
        |> parse
        |> array2D

    //printfn $"%A{grid}"

    let costGrid = costGrid grid

    //printfn $"%A{costGrid}"

    let answerOne = costGrid.[0,0]

    printfn $"Answer for part one is: {answerOne}"

    0
