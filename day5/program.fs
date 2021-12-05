module program

open System.IO
open FParsec

type Point =
    { X : int; Y : int }

    static member fromTuple (x, y) =
        { X = x; Y = y }

type Line =
    { A : Point; B : Point }

type Field =
    { Data : int[,]; Max : Point }

let parseFile path =
    let parseXY =
        (pint32 .>> pchar ',' .>>. pint32)
        |>> Point.fromTuple

    let parseLine =
        (parseXY .>> spaces .>> pstring "->" .>> spaces .>>. parseXY)
        |>> (fun t -> { A = fst t; B = snd t })

    let parse =
        sepEndBy1 parseLine spaces .>> eof

    runParserOnFile parse () path System.Text.Encoding.UTF8

let maxCoord lines =
    List.fold
        (fun (x, y) l ->
            max x (max l.A.X l.B.X),
            max y (max l.A.Y l.B.Y))
        (0, 0)
        lines
    |> Point.fromTuple

let (|DiagonalLine|NonDiagonalLine|) line =
    if line.A.X = line.B.X || line.A.Y = line.B.Y then
        NonDiagonalLine line
    else
        DiagonalLine line

let punchCoords line =
    let range a b =
        let min, max = min a b, max a b
        if min = a then
            seq {for i in 0 .. max - min -> a + i}
        else
            seq {for i in 0 .. max - min -> a - i}

    Seq.map
        Point.fromTuple
        <|  match line with
            | DiagonalLine _ ->
                Seq.zip
                    (range line.A.X line.B.X)
                    (range line.A.Y line.B.Y)
            | NonDiagonalLine _ ->
                seq {for x in range line.A.X line.B.X do
                        for y in range line.A.Y line.B.Y ->
                            (x, y)}

let punchField field line =
    let copy = { field with Data = Array2D.copy field.Data }
    for coord in punchCoords line do
        let prevValue = copy.Data[coord.X, coord.Y]
        copy.Data[coord.X, coord.Y] <- prevValue + 1
    copy

let countOverlapPoints field =
    let mutable count = 0
    Array2D.iter
        (fun x -> if x >= 2 then count <- count + 1; () else ())
        field.Data
    count

[<EntryPoint>]
let main _ =
    match parseFile @"input" with
    | Failure(error, _, _) -> failwith error
    | Success(result, _, _) ->
        let lines = result

        let maxCoords = maxCoord lines

        let field = {
            Data = Array2D.create (maxCoords.X + 1) (maxCoords.Y + 1) 0
            Max = maxCoords
        }

        let nonDiagonalLines =
            List.where
                (fun line ->
                    match line with
                    | NonDiagonalLine _ -> true
                    | _ -> false)
                lines

        let fieldNonDiagonal =
            List.fold punchField field nonDiagonalLines

        let fieldAll =
            List.fold punchField field lines

        let ``answer one`` = countOverlapPoints fieldNonDiagonal
        let ``answer two`` = countOverlapPoints fieldAll

        printfn $"Answer for part one is: {``answer one``}"
        printfn $"Answer for part two is: {``answer two``}"

        0
