#r "nuget: FParsec"

open System

let input =
    IO.File.ReadLines "Day10.txt"
    |> Seq.map List.ofSeq

let openBrackets = ['('; '['; '<'; '{'] |> Set.ofList

let matchBracket c =
    match c with
    | '(' -> ')'
    | '[' -> ']'
    | '<' -> '>'
    | '{' -> '}'
    | _ -> failwith "Not an open bracket"

let scoreCloseBracket c =
    match c with
    | ')' -> 3L
    | ']' -> 57L
    | '>' -> 1197L
    | '}' -> 25137L
    | _ -> failwith "Not a close bracket"

type BadLine =
    | Error of char
    | Incomplete of char list
    | Good

let parseLine originalLine =
    let rec loop (openers : char list) (line : char list) =
        match line with
        | hd :: tl ->
            if Set.contains hd openBrackets then
                loop (hd :: openers) tl
            else
            match openers with
            | hd' :: tl' ->
                if matchBracket hd' = hd then
                    loop tl' tl
                else
                    Error hd
            | _ ->
                Error hd
        | _ -> 
            match openers with
            | [] -> Good
            | _ -> Incomplete openers
    loop [] originalLine

let parsed =
    input |> Seq.map parseLine

let problem1 =
    parsed
    |> Seq.choose (fun x ->
        match x with
        | Error c -> Some c
        | _ -> None)
    |> Seq.sumBy scoreCloseBracket