open System

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type OpCode =
    | Forward of int64
    | Up of int64
    | Down of int64

let parse s =
    match s with
    | Prefix "forward " num -> Forward (Int64.Parse num)
    | Prefix "up " num -> Up (Int64.Parse num)
    | Prefix "down " num -> Down (Int64.Parse num)
    | _ -> failwith "failed to parse"

let input =
    IO.File.ReadAllLines "Day2.txt"
    |> Seq.map parse

let depth =
    input
    |> Seq.fold (fun acc c ->
        acc +
        match c with
        | Up num -> (num * -1L)
        | Down num -> num
        | _ -> 0L) 0L

let horizontal =
    input
    |> Seq.fold (fun acc c ->
        acc +
        match c with
        | Forward num -> num
        | _ -> 0L) 0L

let problem1 = depth * horizontal

let depth2 =
    input
    |> Seq.fold (fun (aim, currDepth) c ->
        match c with
        | Up num -> (aim - num, currDepth)
        | Down num -> (aim + num, currDepth)
        | Forward num -> (aim, currDepth + aim * num)) (0L, 0L)
        |> snd

let problem2 = horizontal * depth2