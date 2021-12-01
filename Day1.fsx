open System

let input =
    IO.File.ReadLines("Day1.txt")
    |> Seq.map Int32.Parse

let numberIncreasing seq =
    seq
    |> Seq.windowed 2
    |> Seq.filter (fun arr -> arr.[0] < arr.[1])
    |> Seq.length

let problem1 = numberIncreasing input

let problem2 =
    input
    |> Seq.windowed 3
    |> Seq.map (Array.reduce (+))
    |> numberIncreasing