module program

open System.IO
open FParsec

//   aaaa
//  b    c
//  b    c
//   dddd
//  e    f
//  e    f
//   gggg

let digiExample =
    [
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
      0b0_1_1_1_1_0_1_1uy ] // 9

let wholesSeq = Seq.initInfinite id

let byteIterator = seq { 0 .. 7 }

let segmentsIterator = seq { 0 .. 6 }

let numbersIterator = seq { 0 .. 9 }

let parse numberMod text =
    let segment = anyOf [ 'a' .. 'g' ]
    let number = many1Chars segment |>> numberMod
    let ws = many (pchar ' ')
    let sep = ws >>. pchar '|' >>. ws
    let numberSeq = sepEndBy number ws
    let line = numberSeq .>> sep .>>. numberSeq
    let lines = sepEndBy line newline

    match run lines text with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let charsToDigi string =
    let map = dict (Seq.zip [ 'a' .. 'g' ] wholesSeq)
    Seq.fold (fun byte char -> 1uy <<< map.[char] ||| byte) 0uy string

let digiToNumber digi =
    match Seq.tryFind (fst >> (=) digi) (Seq.zip digiExample wholesSeq) with
    | Some (_, number) -> Some number
    | None -> None

let countBits digi =
    let mutable n = 0

    for i in byteIterator do
        if (1uy <<< i &&& digi) > 0uy then
            n <- n + 1
        else
            ()

    n

let idMutator = [| 0 .. 6 |]

let mutatedDigi (mutator: int array) digi =
    let mutable acc = 0uy

    for i in segmentsIterator do
        let mut = mutator.[i]
        acc <- 1uy <<< mut &&& digi >>> mut <<< i ||| acc

    acc

let uniqueNumbersSegments =
    digiExample
    |> Seq.zip wholesSeq
    |> Seq.map (fun (n, b) -> n, countBits b)
    |> Seq.countBy snd
    |> Seq.where (snd >> ((=) 1))
    |> Seq.map fst
    |> Seq.toList

let segmentFingerprint mutaror numbers segment =
    let all =
        numbers
        |> List.map (mutatedDigi mutaror)
        |> List.map (fun x -> 1uy <<< segment &&& x)
        |> List.map countBits
        |> List.sum

    let unique =
        numbers
        |> List.where (fun x -> (List.contains (countBits x) uniqueNumbersSegments))
        |> List.sortBy countBits
        |> List.map (mutatedDigi mutaror)
        |> List.map (fun x -> 1uy <<< segment &&& x)
        |> List.map countBits
        |> List.fold (fun (x, n) b -> b <<< n ||| x, n + 1) (0, 0)
        |> fst

    all * unique

let allSegmentsFingerprint mutaror numbers =
    segmentsIterator
    |> Seq.map (segmentFingerprint mutaror numbers)
    |> Seq.toArray

let findMutarorForNumbers (exampleSegmentCounts: int array) numbers =
    let mutator = [| 0 .. 6 |]

    for i in segmentsIterator do
        let wanted = exampleSegmentCounts.[i]

        while let probe = segmentFingerprint mutator numbers i in probe <> wanted do
            mutator.[i] <- (mutator.[i] + 1) % 8

    mutator

let listOfDigitsToNumber list =
    List.foldBack (fun num (acc, n) -> pown 10 n * num + acc, n + 1) list (0, 0)
    |> fst

let solveLine (numbers, phrase) =
    let wantedSegmentsFingerprint =
        allSegmentsFingerprint idMutator digiExample

    let mutator =
        findMutarorForNumbers wantedSegmentsFingerprint numbers

    let phraseDecoded =
        phrase
        |> List.map (mutatedDigi mutator) 
        |> List.map digiToNumber
        |> List.map (fun x ->
            match x with
            | Some x -> x
            | None -> failwith "bad digi")

    listOfDigitsToNumber phraseDecoded

[<EntryPoint>]
let main _ =
    let text = File.ReadAllText @"input"
    let digis = parse charsToDigi text

    let answerOne =
        digis
        |> List.map snd 
        |> List.concat
        |> List.map countBits
        |> List.where (fun x -> List.contains x uniqueNumbersSegments)
        |> List.length

    printfn $"Answer one is {answerOne}"

    let answerTwo = List.map solveLine digis |> List.sum

    printfn $"Answer two is {answerTwo}"

    0
