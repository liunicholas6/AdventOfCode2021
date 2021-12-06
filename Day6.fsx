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
    |> List.collect(fun days ->
        match days with
        | 0 -> [6; 8]
        | x -> [x - 1])

let rec reapply f acc count =
    match count with
    | 0 -> acc
    | _ -> reapply f (f acc) (count - 1)

let problem1 =
    reapply nextday input 80
    |> List.length

let nextday2 (arr : Int64 array) =
    let newarr = Array.zeroCreate 9
    let zeroes = arr.[0]
    for i in List.rev [1..8] do
        newarr.[i - 1] <- arr.[i]
    newarr.[6] <- newarr.[6] + zeroes
    newarr.[8] <- zeroes
    newarr

let problem2 =
    let arr = Array.zeroCreate 9
    input
    |> List.countBy id
    |> List.iter (fun (timer, count) -> arr.[timer] <- count |> Convert.ToInt64)
    reapply nextday2 arr 256
    |> Array.sum