#r "nuget: FParsec"

open System
open FParsec

module Parsing =

    type Parser<'t> = Parser<'t, unit>

    let parse p str =
        match run p str with
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) -> failwith errorMsg

    let parseByLine f filepath =
        IO.File.ReadLines(filepath)
        |> Seq.map f

    let parseWholeString f filepath =
        f IO.File.ReadAllLines