module program

open System
open System.IO

let fishesLife fishes =
    match fishes with
    | [ n0; n1; n2; n3; n4; n5; n6;      n7; n8 ] ->
      [ n1; n2; n3; n4; n5; n6; n7 + n0; n8; n0 ]
    | _ -> failwith "bad format"

let zip fishes =
    Seq.countBy id (Seq.concat [ fishes; [ 0L .. 8L ] ])
    |> Seq.sort
    |> Seq.map (snd >> (fun x -> x - 1) >> int64)
    |> Seq.toList

let unzip zipped =
    Seq.zip zipped (seq { 0L .. Seq.length zipped })
    |> Seq.map (fun (n, x) -> Seq.replicate (int n) x)
    |> Seq.concat
    |> Seq.toList

let rec repeat f state n =
    if n > 0 then
        repeat f (f state) (n - 1)
    else
        state

[<EntryPoint>]
let main _ =
    let fishes =
        (File.ReadAllText @"input")
            .Split([| ',' |])
        |> Array.map Int64.Parse
        |> Array.toList
        |> zip

    let answerOne =
        List.reduce (+) (repeat fishesLife fishes 80)

    printfn $"Answer one is {answerOne}"

    let answerTwo =
        List.reduce (+) (repeat fishesLife fishes 256)

    printfn $"Answer two is {answerTwo}"

    0
