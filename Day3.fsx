open System

let parse s = s

let input =
    IO.File.ReadLines "Day3.txt"
    |> Seq.map (fun s -> s |> seq |> Seq.toArray)
    |> Seq.toArray

let mostCommon (array : char[][]) index =
    (array2D array).[*, index]
        |> Seq.fold (fun acc hd ->
            acc +
            match hd with
            | '0' -> -1
            | '1' -> 1
            | _ -> failwith "oh no") 0
        |> fun i -> if i >= 0 then '1' else '0'

let mostCommonInInput =
    {0 .. 11}
    |> Seq.map (mostCommon input)

let rate i =
    mostCommonInInput
    |> Seq.map (fun bit -> if bit = i then '0' else '1')
    |> String.Concat
    |> fun s -> Convert.ToInt64(s, 2)

let problem1 =
    let gamma = rate '1'
    let epsilon = rate '0'
    gamma * epsilon

let rate2 f =
    (input, {0 .. 11})
    ||> Seq.fold (fun acc hd ->
        match Array.tryExactlyOne acc with
        | None -> 
            Array.filter (fun (arr : char[]) ->
                f arr.[hd] (mostCommon acc hd)) acc
        | _ -> acc
    )
    |> Array.exactlyOne
    |> String.Concat
    |> fun s -> Convert.ToInt64(s, 2)

let problem2 =
    let oxygen = rate2 (=)
    let CO2 = rate2 (<>)
    oxygen * CO2