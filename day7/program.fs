module program

open System
open System.IO

let integral x = Seq.reduce (+) (seq { x .. -1 .. 0 })

let fuelNeededForPose x poses fuelMod =
    Array.map ((fun p -> p - x) >> abs >> fuelMod) poses

let bestPose poses min max fuelMod =
    Array.minBy (fun x -> Array.sum (fuelNeededForPose x poses fuelMod)) [| min .. max |]

let overAllFuelNeeded pose poses fuelMod =
    Array.sum (fuelNeededForPose pose poses fuelMod)

[<EntryPoint>]
let main _ =
    let poses =
        (File.ReadAllText @"input").Split(',')
        |> Array.map Int32.Parse

    let min, max = Array.min poses, Array.max poses

    let soluiton name fuelMod =
        let bestPose = bestPose poses min max fuelMod

        let overAllFuelNeeded = overAllFuelNeeded bestPose poses fuelMod

        printfn $"Solution {name}. Best pose is {bestPose}; Fuel needed {overAllFuelNeeded}"

    soluiton "one" id
    soluiton "two" integral

    0
