#r "nuget: FParsec"

open System
open System.Collections.Generic
open FParsec
type Parser<'t> = Parser<'t, unit>

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

let initial = "PHVCVBFHCVPFKBNHKNBO"

let parser : Parser<_> =
    pipe3 (anyChar) (anyChar) (pstring " -> " >>. anyChar)
        (fun left right center-> ((left, right), center))

let input =
    System.IO.File.ReadAllLines "Day13.txt"
    |> Seq.map(parse parser)
    |> Seq.fold(fun acc (key, value) -> Map.add key value acc) Map.empty

let step (s : String) =
    s.[0] ::
        (s
        |> Seq.pairwise
        |> Seq.collect(fun (left, right) -> [Map.find (left, right) input; right])
        |> Seq.toList)
    |> String.Concat

let rec reapply f acc count =
    match count with
    | 0 -> acc
    | _ -> reapply f (f acc) (count - 1)

let problem1 =
    let counts = 
        reapply step initial 10
        |> Seq.countBy id
    let min =
        counts
        |> Seq.minBy(fun (_, y) -> y)
        |> snd
    let max =
        counts
        |> Seq.maxBy(fun (_, y) -> y)
        |> snd
    max - min