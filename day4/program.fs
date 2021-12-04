module program

open System
open System.IO
open FParsec

let parse text =
    let parserIntComma = 
        sepBy1 pint32 (pchar ',')

    let parseIntSpace =
        many1 (many (pchar ' ') >>. pint32)
        //|>> List.toArray

    let parseGrid =
        sepEndBy1 parseIntSpace newline 
        //|>> List.toArray

    let parser =
        parserIntComma 
        .>> spaces
        .>>. sepEndBy1 parseGrid spaces
        .>> spaces
        .>> eof

    match run parser text with
    | Success (result, _, _) -> fst result, snd result
    | Failure (error, _, _) -> failwith error

let punchGrid number =
    List.map
        (List.map
            (fun x -> if x = number then 0 else x))

let punchGrids number =
    List.map (punchGrid number)

let winnerGrid grid =
    // TODO: use List.permute for rotation
    match List.tryFind (List.forall ((=) 0)) grid with
    | Some row -> true
    | None -> false

let rec solve numbers grids =
    match numbers with
    | [] -> failwith "no more numbers"
    | x :: xr ->
        let newGrids = punchGrids x grids
        match List.tryFind winnerGrid newGrids with
        | Some grid -> grid
        | None -> solve xr newGrids


[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input_example"

    let numbers, grids = parse text

    printfn $"numbers %A{numbers}"
    printfn $"grids %A{grids}"

    let solve = solve numbers grids

    printfn $"solve %A{solve}"

    0
