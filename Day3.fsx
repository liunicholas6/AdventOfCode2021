open System

let parse s = s

let input =
    IO.File.ReadLines "Day3.txt"
    |> Seq.map (fun s -> s |> Seq.toArray)
    |> Seq.toArray

let mostCommonAtIndex index (array: char [] []) =
    (array2D array).[*, index]
    |> Seq.fold
        (fun acc hd ->
            acc
            + match hd with
              | '0' -> -1
              | '1' -> 1
              | _ -> failwith "oh no")
        0
    |> fun i -> if i >= 0 then '1' else '0'

let rate =
    let mostCommonInInput =
        input.[0]
        |> Array.mapi (fun i _ -> mostCommonAtIndex i input)

    fun i ->
        mostCommonInInput
        |> Seq.map (fun bit -> if bit = i then '0' else '1')
        |> String.Concat
        |> fun s -> Convert.ToInt64(s, 2)

let problem1 =
    let gamma = rate '1'
    let epsilon = rate '0'
    gamma * epsilon

let rate2 f =

    let rec loop (index: int) (remaining: char [] []) =
        match Array.tryExactlyOne remaining with
        | Some final -> final
        | None ->
            Array.filter (fun (arr: char []) -> f arr.[index] (mostCommonAtIndex index remaining)) remaining
            |> loop (index + 1)

    input
    |> loop 0
    |> String.Concat
    |> fun s -> Convert.ToInt64(s, 2)

let problem2 =
    let oxygen = rate2 (=)
    let CO2 = rate2 (<>)
    oxygen * CO2
