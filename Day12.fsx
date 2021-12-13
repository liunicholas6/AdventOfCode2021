#r "nuget: FSharp.FGL"

open System
open FSharp.FGL
open FSharp.FGL.Directed

let input =
    System.IO.File.ReadLines "Day12.txt"
    |> Seq.map(fun s ->
    s.Split('-')
    |> fun arr -> (arr.[0], arr.[1]))

type CaveType =
    | Start
    | End
    | Small
    | Large

let getCaveType s =
    match s with
    | "start" -> Start
    | "end" -> End
    | _ ->
        if s |> Seq.fold(fun acc c -> acc && Char.IsLower c) true then Small else Large

type Vertex = LVertex<string, CaveType>

type Edge = LEdge<string, unit>

let vertexList : Vertex list =
    input
    |> Seq.fold(fun acc (v1, v2) -> v1 :: v2 :: acc) []
    |> Seq.distinct
    |> Seq.map(fun v -> (v, getCaveType v))
    |> List.ofSeq

let edgeList : Edge list =
    input
    |> Seq.map (fun (v1, v2) -> (v1, v2, ()))
    |> List.ofSeq

let graph =
    Undirected.Graph.create vertexList edgeList

let getNeighbors v g =
    let (_, _, _, neighbors) = Graph.getContext v g
    neighbors
    |> List.map (fun (name, _) ->
        let (_, _, cavetype, _) = Graph.getContext name g
        (name, cavetype))

type Path = {
    LastVertex : string
    Rest : string list
}

let extend (vertex : string) (path : Path) =
    {LastVertex = vertex; Rest = path.LastVertex :: path.Rest}

let getAllPaths1 g =
    let mutable completePaths = []
    let rec loop (path : Path) =
        getNeighbors path.LastVertex g
            |> Seq.iter(fun (name, cavetype) ->
            match cavetype with
            | Start -> ()
            | End -> completePaths <- (extend name path) :: completePaths
            | Small ->
                if not(List.contains name path.Rest) then loop (extend name path)
            | Large -> loop (extend name path)
        )
    loop {LastVertex = "start"; Rest = []}
    completePaths

let problem1 =
    getAllPaths1 graph
    |> List.length


let getAllPaths2 g =
    let mutable completePaths = []
    let rec loop (path : Path) (doubleVisited : bool) =
        getNeighbors path.LastVertex g
            |> Seq.iter(fun (name, cavetype) ->
            match cavetype with
            | Start -> ()
            | End -> completePaths <- (extend name path) :: completePaths
            | Small ->
                if List.contains name path.Rest then
                    if not doubleVisited then
                        loop (extend name path) true
                else loop (extend name path) doubleVisited
            | Large -> loop (extend name path) doubleVisited    
        )
    loop {LastVertex = "start"; Rest = []} false
    completePaths

let problem2 =
    getAllPaths2 graph
    |> List.length