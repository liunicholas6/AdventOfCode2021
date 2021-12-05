#r "nuget: FParsec"

open FParsec
open System
type Parser<'t> = Parser<'t, unit>

let parser : Parser<_> =
    pipe4 (pfloat) (pstring "," >>. pfloat) (pstring " -> " >>. pfloat) (pstring "," >>. pfloat)
        (fun x1 y1 x2 y2 -> (Convert.ToInt32 x1, Convert.ToInt32 y1), (Convert.ToInt32 x2, Convert.ToInt32 y2))

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

let input =
    IO.File.ReadLines "Day5.txt"
    |> Seq.map (parse parser)

type LineType =
    | Vertical
    | Horizontal
    | Diagonal

type Line = {
    Type : LineType
    Points : (int * int) list
}

let allBetween first last =
    if first < last then
        [first .. last]
    else
        List.rev [last .. first]

let lines =
    input
    |> Seq.map (fun line ->
        let (x1, y1), (x2, y2) = line
        if x1 = x2 then
            {Type = Horizontal; Points = allBetween y1 y2 |> List.map (fun y -> (x1, y))}
        elif y1 = y2 then
            {Type = Vertical; Points = allBetween x1 x2 |> List.map (fun x -> (x, y1))}
        else
            {Type = Diagonal; Points = List.zip (allBetween x1 x2) (allBetween y1 y2)}
    )

let countIntersections lines =
    lines
    |> Seq.map (fun line -> line.Points)
    |> Seq.reduce List.append
    |> List.countBy id
    |> List.filter (fun (_, count) -> count > 1)
    |> List.length

let problem1 =
    lines
    |> Seq.filter (fun line -> line.Type <> Diagonal)
    |> countIntersections

let problem2 =
    lines
    |> countIntersections