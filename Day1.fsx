open System

let input =
    IO.File.ReadLines("Day1.txt")
    |> Seq.map Int32.Parse

let countIncr seq =
    seq
    |> Seq.pairwise
    |> Seq.filter (fun (a, b) -> b > a)
    |> Seq.length

let problem1 = countIncr input

let problem2 =
    input
    |> Seq.windowed 3
    |> Seq.map (Array.reduce (+))
    |> countIncr
