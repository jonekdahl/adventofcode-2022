let guide =
    System.IO.File.ReadAllLines("input")
    |> Seq.map (fun s -> s.Chars(0), s.Chars(2))

type RPS =
    | Rock
    | Paper
    | Scissors

let decodeOpponent = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwith "Unexpected"

let decodeStrategy1 = function
    | 'X' -> Rock
    | 'Y' -> Paper
    | 'Z' -> Scissors
    | _ -> failwith "Unexpected"

let decode1 (c1: char, c2: char) =
    decodeOpponent c1, decodeStrategy1 c2

let score (opponentsChoice: RPS, myChoice: RPS) =
    let choiceScore =
        match myChoice with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let winningScore =
        match myChoice, opponentsChoice with
        | Paper, Rock
        | Scissors, Paper
        | Rock, Scissors -> 6
        | Rock, Paper
        | Paper, Scissors
        | Scissors, Rock -> 0
        | Rock, Rock 
        | Paper, Paper
        | Scissors, Scissors -> 3
    choiceScore + winningScore

guide
|> Seq.map decode1
|> Seq.sumBy score
|> fun score -> printfn $"Part 1: %d{score}"

type Outcome =
    | Lose
    | Draw
    | Win

let decodeStrategy2 = function
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> failwith "Unexpected"


let decode2 (c1: char, c2: char) =
    let opponentsChoice = decodeOpponent c1
    let outcome = decodeStrategy2 c2
    let myChoice =
        match outcome with
        | Win ->
            match opponentsChoice with
            | Rock -> Paper
            | Paper -> Scissors
            | Scissors -> Rock
        | Lose ->
            match opponentsChoice with
            | Rock -> Scissors
            | Paper -> Rock
            | Scissors -> Paper
        | Draw ->
            opponentsChoice
    opponentsChoice, myChoice

guide
|> Seq.map decode2
|> Seq.sumBy score
|> fun score -> printfn $"Part 2: %d{score}"
