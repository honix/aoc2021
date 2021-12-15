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

let iterateVertical grid =
    seq {
        let rowMax = Array2D.length1 grid - 1
        let colMax = Array2D.length2 grid - 1
        for row in 0 .. rowMax do
            for col in 0 .. colMax do
                yield row, col
    }

let iterateVerticalBack grid =
    Seq.rev (iterateVertical grid)

let iterateHorizontal grid = 
    seq {
        let rowMax = Array2D.length1 grid - 1
        let colMax = Array2D.length2 grid - 1
        for col in 0 .. colMax do
            for row in 0 .. rowMax do
                yield row, col
    }

let iterateHorizontalBack grid =
    Seq.rev (iterateHorizontal grid)

let iterateDiag grid =
    seq {
        let rowMax = Array2D.length1 grid - 1
        let colMax = Array2D.length2 grid - 1
        for diag in 0 .. rowMax do
            let mutable row = diag
            let mutable col = 0
            while isCoordsInBounds grid row col do
                yield row, col
                row <- row - 1
                col <- col + 1
        for diag in 1 .. colMax do
            let mutable row = rowMax
            let mutable col = diag
            while isCoordsInBounds grid row col do
                yield row, col
                row <- row - 1
                col <- col + 1
    }

let iterateDiagBack grid =
    Seq.rev (iterateDiag grid)

let iterateDiagBackSkip1 grid =
    Seq.skip 1 (iterateDiagBack grid)

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

    let maxRow, maxCol =
        Array2D.length1 grid - 1,
        Array2D.length2 grid - 1

    costGrid.[maxRow, maxCol] <- grid.[maxRow, maxCol]

    let punch (row, col) =
        let closeMin = closeMin costGrid row col
        costGrid.[row, col] <-
            min
                (costGrid.[row, col])
                (closeMin + grid.[row, col])

    let punchIter iter =
        Seq.iter punch (iter costGrid)

    punchIter iterateDiagBackSkip1

    let circle = [
        iterateHorizontalBack
        iterateDiag
        iterateDiagBack
        iterateHorizontal
    ]

    // One might need more than 3 turns to solve
    for _ in 1 .. 3 do
        List.iter punchIter circle

    costGrid.[0, 0] <- closeMin costGrid 0 0

    costGrid

let valueMap size value =
    seq {
        for i in 0 .. size - 1 do
            seq {
                for j in 0 .. size - 1 do
                yield (value - 1 + i + j) % 9 + 1
            }
    }
    |> array2D

[<EntryPoint>]
let main _ =
    let grid =
        File.ReadAllText @"input"
        |> parse
        |> array2D

    //printfn $"%A{grid}"

    let costSmallGrid = costGrid grid

    //printfn $"%A{costSmallGrid}"

    let answerOne = costSmallGrid.[0,0]

    printfn $"Answer for part one is: {answerOne}"

    let largeGrid =
        let valueMap = Array2D.map (valueMap 5) grid

        let smallRow, smallCol =
            Array2D.length1 grid,
            Array2D.length2 grid

        Array2D.init
            (smallRow * 5)
            (smallCol * 5)
            (fun row col ->
                let valueMap = valueMap.[row % smallRow, col % smallCol]
                valueMap.[row / smallRow, col / smallCol])

    //printfn $"%A{largeGrid}"

    let costLargeGrid = costGrid largeGrid

    //printfn $"%A{costLargeGrid}"

    let answerTwo = costLargeGrid.[0,0]

    printfn $"Answer for part two is: {answerTwo}"

    0
