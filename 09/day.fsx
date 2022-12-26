type Direction =
    | Up
    | Down
    | Left
    | Right
with
    static member create = function
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | other -> failwith $"Not a direction: %s{other}"

    member this.vector =
        match this with
        | Up -> (0, 1)
        | Down -> (0, -1)
        | Left -> (-1, 0)
        | Right -> (1, 0)


let motions =
    System.IO.File.ReadAllLines("input.txt")
    |> Seq.map (fun s -> 
        match s.Split(" ") with
        | [| direction; steps |] -> 
            (direction |> Direction.create, steps |> int)
        | other -> 
            failwith $"Unexpected input: %A{other}")


let isTouching head tail =
    let hx, hy = head
    let tx, ty = tail

    abs (hx - tx) <= 1 && abs (hy - ty) <= 1

let moveHead head (direction: Direction) =
    let hx, hy = head
    let dhx, dhy = direction.vector
    hx + dhx, hy + dhy

let moveTail head tail =
    if isTouching head tail then
        tail
    else        
        let tx, ty = tail
        let nhx, nhy = head
        let difftx = nhx - tx
        let diffty = nhy - ty
        let dtx = if difftx = 0 then 0 else difftx / (abs difftx)
        let dty = if diffty = 0 then 0 else diffty / (abs diffty)
        let newTail = tx + dtx, ty + dty
        newTail

let simulate motions (ropeLength: int) =
    let knots = Array.init ropeLength (fun _ -> (0, 0))
    let mutable tailPositions = Set [ Array.last knots ]

    for m in motions do
        let direction, count = m
        for _ in 1..count do
            knots[0] <- moveHead knots[0] direction
            for i in 0..knots.Length-2 do
                let head, tail = knots[i], knots[i+1]
                let newTail = moveTail head tail
                knots[i+1] <- newTail
            tailPositions <- tailPositions.Add <| Array.last knots

    tailPositions.Count

simulate motions 2
|> printfn "Part 1: %A"


simulate motions 10
|> printfn "Part 2: %A"

