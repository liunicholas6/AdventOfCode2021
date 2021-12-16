#r "nuget: FParsec"

open FParsec
type Parser<'t> = Parser<'t, unit>

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

let startString = "PHVCVBFHCVPFKBNHKNBO"

let parser: Parser<_> =
    pipe3 (anyChar) (anyChar) (pstring " -> " >>. anyChar) (fun left right center -> (left, right), center)

let pairMapping key =
    System.IO.File.ReadAllLines "Day14.txt"
    |> Seq.map (parse parser)
    |> Map
    |> Map.find key

let addToMap (key: 'a) (value: int64) (map: Map<'a, int64>) =
    map
    |> Map.change key (fun x ->
        match x with
        | Some s -> Some(s + value)
        | None -> Some value)

let startMap =
    startString
    |> Seq.pairwise
    |> Seq.fold (fun map pair -> map |> addToMap pair 1L) Map.empty

let step (map: Map<char * char, int64>) =
    map
    |> Map.fold
        (fun newMap (left, right) count ->
            let center = pairMapping (left, right)

            newMap
            |> addToMap (left, center) count
            |> addToMap (center, right) count)
        Map.empty

let rec reapply f count acc =
    match count with
    | 0 -> acc
    | _ -> reapply f (count - 1) (f acc)

let charCounts =
    let edgeChars =
        [ (startString.[0], 1L)
          (startString.[startString.Length - 1], 1L) ]
        |> Map

    fun (map: Map<char * char, int64>) ->
        map
        |> Map.fold (fun acc (left, right) twiceCount -> acc |> addToMap left twiceCount |> addToMap right twiceCount) edgeChars
        |> Map.map (fun _ twiceCount -> twiceCount / 2L)

let countDiffs l = (l |> Seq.max) - (l |> Seq.min)

let solveProblem repeats =
    startMap
    |> reapply step repeats
    |> charCounts
    |> Seq.map (fun entry -> entry.Value)
    |> countDiffs

let problem1 = solveProblem 10

let problem2 = solveProblem 40
