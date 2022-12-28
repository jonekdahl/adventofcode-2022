
type Operand = 
    | Old
    | Value of int64
with
    static member parse = function
        | "old" -> Old
        | other -> Value (int64<string> other)

    member this.eval old =
        match this with
        | Old -> old
        | Value v -> v


type Operator =
    | Plus
    | Times
with
    static member parse = function
        | "+" -> Plus
        | "*" -> Times
        | other -> failwith $"Unknown operator: {other}"

type Operation = {
    operator: Operator
    op1: Operand
    op2: Operand
}
with
    static member parse (s: string) =
        match s.Split(" ") with
        | [| s1; s2; s3 |] ->
            {
                op1      = Operand.parse s1
                operator = Operator.parse s2
                op2      = Operand.parse s3
            }
        | _ -> failwith "Unexpected operation"

    member this.eval (old: int64) =
        match this.operator with
        | Plus  -> (this.op1.eval old) + (this.op2.eval old)
        | Times -> (this.op1.eval old) * (this.op2.eval old)

type Monkey = {
    idx: int
    items: int64 array
    operation: Operation
    divisableBy: int64
    ifTrue: int
    ifFalse: int
}

// 0: Monkey 0:
// 1:   Starting items: 79, 98
// 2:   Operation: new = old * 19
// 3:   Test: divisible by 23
// 4:     If true: throw to monkey 2
// 5:     If false: throw to monkey 3

let parseMonkey (s: string) =
    let lines = s.Split("\n")
    {
        idx          = (lines[0].Remove(0, "Monkey ".Length).TrimEnd(':')) |> int<string>
        items        = (lines[1].Remove(0, "  Starting items: ".Length)).Split(", ") |> Array.map int64<string>
        operation    = (lines[2].Remove(0, "  Operation: new = ".Length)) |> Operation.parse
        divisableBy  = (lines[3].Remove(0, "  Test: divisible by ".Length)) |> int64<string>
        ifTrue       = (lines[4].Remove(0, "    If true: throw to monkey ".Length)) |> int<string>
        ifFalse      = (lines[5].Remove(0, "    If false: throw to monkey ".Length)) |> int<string>
    }

type Part = Part1 | Part2

let run (part: Part) =
    let monkeys = 
        System.IO.File.ReadAllText("input.txt").Split("\n\n")
        |> Seq.map parseMonkey
        |> Array.ofSeq

    let modAll = monkeys |> Seq.fold (fun acc m -> acc * m.divisableBy) 1L

    let monkeyActive: int64[] = Array.zeroCreate monkeys.Length

    let playRound () =
        let inspectAndThrow (monkey: Monkey) oldWorryLevel =
            // Register activity
            monkeyActive[monkey.idx] <- monkeyActive[monkey.idx] + 1L

            let worryLevelAfterInspection = monkey.operation.eval oldWorryLevel
            let worryLevelAfterRelief = 
                match part with
                | Part1 -> worryLevelAfterInspection / 3L
                | Part2 -> worryLevelAfterInspection % modAll
            let targetMonkey = if worryLevelAfterRelief % monkey.divisableBy = 0 then monkey.ifTrue else monkey.ifFalse
            monkeys[targetMonkey] <- {
                monkeys[targetMonkey] with
                    items = (Array.append (monkeys[targetMonkey].items) [| worryLevelAfterRelief |]) }
    
        monkeys
        |> Array.iteri (fun idx monkey ->
            monkey.items
            |> Seq.iter (inspectAndThrow monkey)
            monkeys[idx] <- { monkeys[idx] with items = [||] }
        )

    let rounds =
        match part with
        | Part1 -> 20
        | Part2 -> 10_000
    
    seq { 1 .. rounds }
    |> Seq.iter (fun round -> 
        playRound ()
        //printfn "After round %d, then monkeys are holding these items:" round
        //monkeys 
        //|> Seq.iter (fun monkey -> 
        //    printfn "Monkey %d: %A" monkey.idx monkey.items)
    )

    monkeyActive

let monkeyBusiness (monkeyActive: int64[]) =
    monkeyActive
    |> Array.sortDescending
    |> fun act -> act[0] * act[1]

run Part1
|> monkeyBusiness
|> printfn "Part 1: %d"

run Part2
|> monkeyBusiness
|> printfn "Part 2: %d"
