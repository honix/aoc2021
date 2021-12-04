module program

open System
open System.IO

let countRises numbers =
    let rises =
        Seq.windowed 2 numbers
        |> Seq.map (fun c -> c[0] < c[1])

    Seq.where id rises |> Seq.length


[<EntryPoint>]
let main _ =
    let numbers =
        File.ReadLines @"input"
        |> Seq.map Int32.Parse

    let ``part one`` =
        numbers
        |> countRises

    let ``part two`` =
        Seq.windowed 3 numbers
        |> Seq.map (Array.reduce (+))
        |> countRises

    printfn $"Answer for part one is: {``part one``}"
    printfn $"Answer for part two is: {``part two``}"

    0
