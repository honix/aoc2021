module program

open System
open System.IO
open FParsec

//   aaaa
//  b    c
//  b    c
//   dddd
//  e    f
//  e    f
//   gggg

let digiExample = [
    //  a b c d e f g
    0b0_1_1_1_0_1_1_1uy // 0
    0b0_0_0_1_0_0_1_0uy // 1
    0b0_1_0_1_1_1_0_1uy // 2
    0b0_1_0_1_1_0_1_1uy // 3
    0b0_0_1_1_1_0_1_0uy // 4
    0b0_1_1_0_1_0_1_1uy // 5
    0b0_1_1_0_1_1_1_1uy // 6
    0b0_1_0_1_0_0_1_0uy // 7
    0b0_1_1_1_1_1_1_1uy // 8
    0b0_1_1_1_1_0_1_1uy // 9
]

// //let easyNumbers = [ 1; 4; 7; 8 ]
// let uniqueNumbers = [ 
//     2 // 1 has segments uniquely
//     4 // 4 has segments uniquely
//     3 // 7 has segments uniquely
//     7 // 8 has segments uniquely
// ]

let wholesSeq =
    Seq.initInfinite id

let parse numberMod text =
    let segment = anyOf [ 'a' .. 'g' ]
    let number = many1Chars segment |>> numberMod
    let ws = many (pchar ' ')
    let sep = ws >>. pchar '|' >>. ws
    let numberSeq = sepEndBy number ws
    let line = numberSeq .>> sep .>>. numberSeq
    let lines = sepEndBy line newline

    match run lines text with
    | Success(result,_,_) -> result
    | Failure(error,_,_) -> failwith error

let charsToDigi string =
    let map = dict (Seq.zip [ 'a' .. 'g' ] wholesSeq)
    Seq.fold (fun byte char -> 1uy <<< map[char] ||| byte) 0uy string 

let countBits digi =
    let mutable n = 0
    for i in seq { 0 .. 7 } do
        if (1uy <<< i &&& digi) > 0uy then
            n <- n + 1
        else ()
    n

[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input_example"
    let digis = parse charsToDigi text

    let uniqueNumbersSegments =
        digiExample
        |> Seq.zip wholesSeq
        |> Seq.map (fun (n, b) -> n, countBits b)
        |> Seq.countBy snd 
        |> Seq.where (snd >> ((=) 1))
        |> Seq.map fst
        |> Seq.toList

    printfn $"%A{uniqueNumbersSegments}"

    let answerOne = 
        List.map snd digis 
        |> List.concat
        |> List.map countBits
        |> List.where (fun x -> List.contains x uniqueNumbersSegments)
        |> List.length
    
    printfn $"{answerOne}"

    0
