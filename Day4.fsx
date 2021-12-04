open System
let parse (s : string) =
    s.Split("\r\n\r\n")
        |> Array.map (fun s ->
            s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s ->
                s.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map Int32.Parse
            )
        )

let input = System.IO.File.ReadAllText "Day4.txt" |> parse

let drawn_numbers = Array.exactlyOne input.[0] |> Array.toList

type BoardSquare = {
    Value : int
    mutable Marked : bool
}

type Board = {
    Squares : BoardSquare[,]
    mutable Score : int option
} 

let getInitialBoards =
    fun () ->
        input.[1..]
        |> Array.map (fun arr ->
            arr
            |> array2D
            |> Array2D.map (fun x ->
                {Value = x; Marked = false})
            |> fun x -> {Squares = x; Score = None}
        ) |> Array.toList

let playTurn (drawn_number : int) (boards: Board list)=
    let isWinningTurn (board : Board) (i : int) (j : int) =
        let isWinningLine (line : BoardSquare[]) =
            Array.forall (fun square -> square.Marked) line
        isWinningLine board.Squares.[i, *] || isWinningLine board.Squares.[*, j]

    let score (board : Board) (drawn_number : int) =
        let mutable sum = 0
        board.Squares
        |> Array2D.iter (fun v ->
            if not v.Marked then
                sum <- sum + v.Value)
        sum * drawn_number

    for board in boards do
        board.Squares
        |> Array2D.iteri (fun i j v ->
            if v.Value = drawn_number then
                v.Marked <- true
            if isWinningTurn board i j && board.Score = None then board.Score <- Some (score board drawn_number))
    boards

let problem1 =
    let boards = getInitialBoards()
    let rec loop (numbers : int list) (boards : Board list)=
        match numbers with
        | drawn_number :: rest ->

            boards
            |> playTurn drawn_number
            |> List.choose (fun board -> board.Score)
            |> List.tryExactlyOne
            |> fun x ->
                match x with
                | Some result -> result
                | None -> loop rest boards

        | [] -> failwith "Problem1 ran out of numbers"
    loop drawn_numbers boards

let problem2 =
    let initial_boards = getInitialBoards()
    let rec loop (numbers : int list) (boards : Board list) =
        match numbers with
        | drawn_number :: rest ->
            boards
            |> List.filter (fun board -> board.Score = None)
            |> playTurn drawn_number
            |> fun x ->
                match x with
                    | [finalBoard] ->
                        match finalBoard.Score with
                        | Some score -> score
                        | None -> loop rest x
                    | _ -> loop rest x
        | [] -> failwith "Problem2 ran out of numbers"
    loop drawn_numbers initial_boards