type Instruction =
    | Noop
    | Addx of int

let parseInstruction (s: string) =
    match s.Split " " with
    | [| "noop" |] -> Noop
    | [| "addx"; v |] -> Addx (int v)
    | other -> failwith $"Could not parse instruction: %A{other}" 


let program = 
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map parseInstruction

type CpuState = {
    ticks: int
    regX: int
}
with
    static member signalStrength (state: CpuState) =
        state.ticks * state.regX
    
    static member toPixel (state: CpuState) = 
        let spriteMiddle = state.regX
        let position = (state.ticks - 1) % 40
        if spriteMiddle >= position - 1 && spriteMiddle <= position + 1 then 'X' else '.'

let executeInstruction (history: CpuState seq) (instruction: Instruction): CpuState seq =
    let state = history |> Seq.last
    Seq.append
        history
        (match instruction with
        | Noop ->
            seq {
                { state with ticks = state.ticks + 1 } 
            }
        | Addx op1 ->
            seq {
                { state with ticks = state.ticks + 1 }
                { state with ticks = state.ticks + 2; regX = state.regX + op1 }
            })
    
let initialState = seq { { ticks = 1; regX = 1 } }
let states =
    program
    |> Seq.fold executeInstruction initialState

states
|> Seq.filter (fun state -> (state.ticks + 20) % 40 = 0)
|> Seq.map CpuState.signalStrength
|> Seq.sum
|> printfn "Part 1: %A"

printfn "Part 2:"
states
|> Seq.map CpuState.toPixel
|> Seq.chunkBySize 40
|> Seq.map System.String
|> Seq.iter (printfn "%s")
