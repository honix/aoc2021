module program

open System.IO
open FParsec

type Grid = {
    Data : int array
    Dims : (int * int)
}

let parse text =
    let parserIntComma =
        sepBy1 pint32 (pchar ',')

    let parseIntSpace =
        many1 (many (pchar ' ') >>. pint32)
        |>> List.toArray

    let parseGrid =
        sepEndBy1 parseIntSpace newline
        |>> (fun x -> {
                Data = Array.concat x
                Dims = (List.head x).Length, x.Length
            })

    let parser =
        parserIntComma
        .>> spaces
        .>>. sepEndBy1 parseGrid spaces
        .>> spaces
        .>> eof

    match run parser text with
    | Success (result, _, _) -> fst result, snd result
    | Failure (error, _, _) -> failwith error

let punchGrid number grid =
    { grid with
        Data = Array.map 
                (fun x -> if x = number then 0 else x) 
                grid.Data }

let punchGrids number =
    List.map (punchGrid number)

let winnerGrid grid =
    let dimX, dimY = grid.Dims

    let mapIds =
        List.map (List.map (Array.get grid.Data))

    let rows =
        mapIds [for i in 0 .. dimY - 1 ->
                [for j in 0 .. dimX - 1 -> i * dimY + j]]

    let columns =
        mapIds [for i in 0 .. dimX - 1 ->
                [for j in 0 .. dimY - 1 -> i + j * dimY]]

    let find patterns =
        match List.tryFind (List.forall ((=) 0)) patterns with
        | Some _ -> true
        | None -> false
    
    find rows || find columns

let rec solve numbers grids =
    match numbers with
    | [] -> failwith "no more numbers, no winner"
    | x :: xr ->
        let newGrids = punchGrids x grids
        match List.tryFind winnerGrid newGrids with
        | Some grid -> grid, x
        | None -> solve xr newGrids


[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input"

    let numbers, grids = parse text

    let solve, x = solve numbers grids

    let ``answer one`` = Array.reduce (+) solve.Data * x

    printfn $"Answer for part one is: {``answer one``}"

    0
