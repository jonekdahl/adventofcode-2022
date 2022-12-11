let lines =
    System.IO.File.ReadAllLines "input.txt"

type Stacks = Map<string, char list>
type Move = int * string * string

let parseCrateLine (line: string): char option[] =
    line
    |> Seq.chunkBySize 4
    |> Seq.map (fun chars -> if chars[1] = ' ' then None else Some chars[1])
    |> Seq.toArray

let parse (lines: string seq) =
    let stackNumbers: string seq =
        lines
        |> Seq.find (fun l -> l.StartsWith(" 1 "))
        |> fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)

    let emptyStacks: Stacks =
        stackNumbers
        |> Seq.map (fun s -> s, [])
        |> Map.ofSeq

    let stacks: Stacks =
        lines
        |> Seq.takeWhile (fun l -> not <| l.StartsWith(" 1 "))
        |> Seq.map parseCrateLine
        |> Seq.rev
        |> Seq.fold (fun (stacks: Stacks) (crates: char option[]) ->
                        Seq.zip stackNumbers crates
                        |> Seq.fold (fun stacks (stackNumber, maybeChar) -> 
                                        match maybeChar with
                                        | Some crate -> stacks.Add(stackNumber, crate :: stacks[stackNumber])
                                        | None -> stacks)
                                    stacks)
                    emptyStacks

    let moves =
        lines
        |> Seq.skipWhile (fun l -> not (l.StartsWith("move ")))
        |> Seq.map (fun l -> 
            let parts = l.Split(" ")
            ((int parts[1]), parts[3], parts[5]))

    stacks, stackNumbers, moves

let moveOne (stacks: Stacks) (source: string) (target: string): Stacks =
    let elem = stacks[source].Head

    stacks
    |> fun stacks -> stacks.Add(source, stacks[source].Tail)
    |> fun stacks -> stacks.Add(target, elem :: stacks[target])

let moveOneAtATime (stacks: Stacks) (move: Move): Stacks =
    let numMoves, source, target = move
    seq { 1..numMoves }
    |> Seq.fold (fun (stacks: Stacks) _ -> moveOne stacks source target) stacks

let moveMany (stacks: Stacks) (move: Move): Stacks =
    let numMoves, source, target = move
    let sourceStack = stacks[source]
    let crates = sourceStack[0..numMoves-1]

    stacks
    |> fun stacks -> stacks.Add(source, sourceStack[numMoves..sourceStack.Length-1])
    |> fun stacks -> stacks.Add(target, crates @ stacks[target])

let topCrates (stackNumbers: string seq) (stacks: Stacks) =
    stackNumbers
    |> Seq.map (fun stackNumber -> stacks[stackNumber].Head)
    |> System.String.Concat


let stacks, stackNumbers, moves = parse lines

moves
|> Seq.fold moveOneAtATime stacks
|> topCrates stackNumbers
|> printfn "Part 1: %s"

moves
|> Seq.fold moveMany stacks
|> topCrates stackNumbers
|> printfn "Part 2: %s"
