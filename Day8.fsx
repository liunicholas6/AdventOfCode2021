#r "nuget: FParsec"

open FParsec
open System

type Parser<'t> = Parser<'t, unit>

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

let input =
    IO.File.ReadLines "Day8.txt"
    |> Seq.map (fun s ->
        s
        |> Seq.map(fun char -> int char - int '0')
        |> Array.ofSeq)
    |> Array.ofSeq
    |> array2D

let removeInvalidIndices =
    List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < Array2D.length1 input && y < Array2D.length2 input)

let getNeighborhood (xPos, yPos) l =
    l
    |> List.map(fun (x, y) -> xPos + x, yPos + y)
    |> removeInvalidIndices

let getMooreHood (xPos, yPos) =
    List.allPairs [-1; 0; 1] [-1; 0; 1]
    |> List.map (fun (x, y) -> xPos + x, yPos + y)
    |> removeInvalidIndices

let cardinalNeighbors (xPos, yPos) =
    [(1, 0); (-1, 0); (0, 1); (0, -1)]
    |> List.map (fun (x, y) -> xPos + x, yPos + y)
    |> removeInvalidIndices

let getMin l =
    l |> List.reduce (fun hd acc -> if hd < acc then hd else acc)

let lowPoints =
    List.allPairs [0 .. (Array2D.length1 input) - 1] [0 .. (Array2D.length2) input - 1]
    |> List.filter(fun (x, y) ->
        input.[x, y] = (getMooreHood (x, y) |> List.map(fun (x, y) -> input.[x, y]) |> getMin))

let problem1 =
    lowPoints
    |> List.map(fun (x, y) -> input.[x, y] + 1)
    |> List.sum

let findBasin (lowpoint : int * int) =
    let rec loop (toCheck : (int * int) list) (seen : (int * int) list) =
        match toCheck with
        | [] -> seen
        | hd::tl ->
            if List.contains hd seen then
                loop tl seen
            else
                let toAdd =
                    hd
                    |> cardinalNeighbors
                    |> List.filter(fun (x, y) -> input.[x, y] <> 9)
                loop (List.append toAdd toCheck) (hd::seen)
    loop [lowpoint] []

let problem2 =
    lowPoints
    |> List.map(fun lowPoint ->
        findBasin lowPoint
        |> List.length)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce(*)