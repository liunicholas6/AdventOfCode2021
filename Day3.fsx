open System

let parse s = s

let input =
    IO.File.ReadAllLines "Day2.txt"
    |> Seq.map parse

let problem1 = 1

let problem2 = 2