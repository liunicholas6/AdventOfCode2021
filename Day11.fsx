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

let removeInvalidIndices arr =
    List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < Array2D.length1 arr && y < Array2D.length2 arr)

let getMooreHood (xPos, yPos) arr =
    List.allPairs [-1; 0; 1] [-1; 0; 1]
    |> List.filter(fun p -> p <> (0, 0))
    |> List.map (fun (x, y) -> xPos + x, yPos + y)
    |> removeInvalidIndices arr

let step old_octopi=

    let octopi =
        old_octopi
        |> Array2D.map(fun i -> i + 1)

    let mutable loopRunning = true
    let mutable flashes = 0

    while loopRunning do
        loopRunning <- false
        octopi
        |> Array2D.iteri (fun i j v ->
            if v > 9 then
                loopRunning <- true
                flashes <- flashes + 1
                octopi.[i, j] <- 0
                getMooreHood (i, j) octopi
                |> List.iter(fun (x, y) -> 
                    if octopi.[x, y] <> 0 then
                        octopi.[x, y] <- octopi.[x, y] + 1))
    
    (flashes, octopi)


let problem1 =
    [1 .. 100]
    |> Seq.fold(fun (flashes, octopi) _ ->
        let (newFlashes, newOctopi) = step octopi
        (flashes + newFlashes, newOctopi)) (0, input)
    |> fst

let problem2 =
    let rec loop count octopi =
        let newOctopi = snd (step octopi)
        let mutable allFlashed = true
        newOctopi
        |> Array2D.iter(fun x -> if x <> 0 then allFlashed <- false)
        if allFlashed then count else loop (count + 1) newOctopi
    
    loop 1 input