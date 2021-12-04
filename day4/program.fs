module program

open System
open System.IO
open FParsec

let parse text =
    let parserIntComma =
        sepBy1 pint32 (pchar ',')

    let parseIntSpace =
        many1 (many (pchar ' ') >>. pint32)

    let parseGrid =
        sepEndBy1 parseIntSpace newline

    let parser =
        parserIntComma .>> newline
        .>> newline
        .>>. sepEndBy1 parseGrid spaces
        .>>? spaces 
        .>>? eof

    match run parser text with
    | Success(result, _, _) ->
        fst result, snd result
    | Failure(error, _, _) ->
        failwith error


[<EntryPoint>]
let main _ =
    let text =
        File.ReadAllText @"input_example"

    let a, b =
        parse text

    printfn $"numbers %A{a}"
    printfn $"grids %A{b}"

    0
