module program

open System.IO

[<EntryPoint>]
let main _ =
    let stringToItem string =
        let charToNumber char =
            match char with
            | '0' -> 0
            | '1' -> 1
            | _ -> failwith $"unknown symbol {char}"

        Seq.map charToNumber string |> Seq.toList

    let countOnes itemLength itemsList =
        let folder accs numbers =
            List.map2 (+) accs numbers

        let zeroState = List.replicate itemLength 0
        List.fold folder zeroState itemsList

    let balanceList count =
        List.map (fun x -> if x >= count / 2 then 1 else 0)

    let invertList =
        List.map (function 0 -> 1 | _ -> 0)

    let encodePartTwo itemLength itemsList inv =
        let rec encodePartTwoRec itemsList depth =
            let mask =
                let makeMask =
                    countOnes itemLength
                    >> balanceList (List.length itemsList)
                    >> if inv then invertList else id

                makeMask itemsList

            match itemsList with
            | [] -> failwith "short itemsList"
            | x :: [] -> x
            | xs ->
                match mask with
                | [] -> failwith "short mask"
                | ms ->
                    let getter = List.item depth
                    let filter item = getter item = getter ms
                    let filtered = List.filter filter xs

                    encodePartTwoRec filtered (depth + 1)

        encodePartTwoRec itemsList 0


    let doit =
        let codes, length, itemLength =
            let array = File.ReadAllLines @"input"
            Array.toList array, array.Length, array[0].Length

        let itemsList =
            List.map stringToItem codes

        let onesCount =
            countOnes itemLength itemsList

        let gammaRateList =
            balanceList length onesCount

        let epsilonRateList =
            invertList gammaRateList

        let oxygenRatingList =
            encodePartTwo itemLength itemsList false

        let co2RatingList =
            encodePartTwo itemLength itemsList true

        let listToInt list =
            let state =
                List.foldBack
                    (fun x (s, i) -> s + (x <<< i), i + 1)
                    list
                    (0, 0)

            fst state

        let ``answer one`` =
            listToInt gammaRateList * listToInt epsilonRateList

        let ``answer two`` =
            listToInt oxygenRatingList * listToInt co2RatingList

        printfn $"Answer for part one is: {``answer one``}"
        printfn $"Answer for part two is: {``answer two``}"

        0

    doit
