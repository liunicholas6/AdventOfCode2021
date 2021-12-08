#r "nuget: FParsec"
#load "Combinatorics.fsx"

open FParsec
open System
open Combinatorics

type Parser<'t> = Parser<'t, unit>

let parse p str =
    match run p str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwith errorMsg

type WireSegment =
    | A | B | C | D | E | F | G

let wireSegmentList = [A; B; C; D; E; F; G]

let charToSegment c =
    match c with
    | 'a' -> A
    | 'b' -> B
    | 'c' -> C
    | 'd' -> D
    | 'e' -> E
    | 'f' -> F
    | 'g' -> G
    | _ -> failwith "Failed to parse char"

let numberMap = 
    [
        [A; B; C; E; F; G];
        [C; F];
        [A; C; D; E; G];
        [A; C; D; F; G];
        [B; C; D; F];
        [A; B; D; F; G];
        [A; B; D; E; F; G];
        [A; C; F];
        [A; B; C; D; E; F; G];
        [A; B; C; D; F; G]
    ]
    |> List.mapi(fun i l -> Set.ofList l, i)
    |> Map

let input =
    IO.File.ReadLines "Day7.txt"
    |> Seq.map (fun s ->
        s.Split('|', StringSplitOptions.TrimEntries)
        |> Array.map (fun (s : string) ->
            s.Split(" ")
            |> Array.map (fun s ->
                s
                |> Seq.map charToSegment
                |> Set.ofSeq
                )
            )
            |> fun arr -> (arr.[0], arr.[1])
        )

let problem1 =
    input
    |> Seq.map (fun pair ->
        snd pair
        |> Array.filter (fun set -> [2; 3; 4; 7] |> List.contains (Set.count set) )
        |> Array.length
        )
    |> Seq.sum

let allMaps =
    permute wireSegmentList
    |> Seq.map (fun permutation ->
        (wireSegmentList, permutation)
        ||> List.zip |> Map)
    |> Seq.cache

let decryptDigit map encryptedDigitDisplay =
    encryptedDigitDisplay
    |> Set.map (fun segment -> map |> Map.find segment)

let problem2 =
    input
    |> Seq.map (fun (key, fourDigitDisplay) ->
        let goodMap = 
            allMaps
            |> Seq.find (fun map ->
                Array.append key fourDigitDisplay
                |> Array.map (decryptDigit map)
                |> Array.forall(fun decryptedDigitDisplay ->
                    numberMap |> Map.containsKey decryptedDigitDisplay)
            )
        fourDigitDisplay
        |> Array.map (fun encryptedDigitDisply ->
            encryptedDigitDisply
            |> decryptDigit goodMap
            |> fun x -> numberMap |> Map.find x)
        |> Array.reduce (fun a b -> a * 10 + b)
    )
    |> Seq.sum