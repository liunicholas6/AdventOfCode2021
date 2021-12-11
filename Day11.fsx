open System
let parse (s : string) =
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s ->
        s
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toArray
    )
    |> Seq.toArray

let input =
    System.IO.File.ReadAllText "Day11.txt"
    |> parse
    |> array2D

let step =
    let rec loop 2DArray