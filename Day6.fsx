#r "nuget: FParsec"

open FParsec
open System

type Parser<'t> = Parser<'t, unit>

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

let input =
    IO.File.ReadAllLines "Day6.txt"
    |> Array.exactlyOne
    |> parse (sepBy pfloat (pstring ","))
    |> List.map (Convert.ToInt32)

let nextday l =
    l
    |> List.collect (fun days ->
        match days with
        | 0 -> [ 6; 8 ]
        | x -> [ x - 1 ])

let rec reapply f acc count =
    match count with
    | 0 -> acc
    | _ -> reapply f (f acc) (count - 1)

let problem1 = reapply nextday input 80 |> List.length

let nextday2 (arr: Int64 array) =
    let zeroes = arr.[0]

    for i in [ 1 .. 8 ] do
        arr.[i - 1] <- arr.[i]

    arr.[6] <- arr.[6] + zeroes
    arr.[8] <- zeroes
    arr

let problem2 =
    let arr = Array.zeroCreate 9

    input
    |> List.countBy id
    |> List.iter (fun (timer, count) -> arr.[timer] <- Convert.ToInt64 count)

    reapply nextday2 arr 256 |> Array.sum
