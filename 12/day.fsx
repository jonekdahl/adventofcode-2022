let grid =
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.ToCharArray())

type Coord = int * int

let find grid (ch: char): Coord =
    let row = grid |> Array.findIndex (fun row -> row |> Seq.contains ch)
    let col = grid[row] |> Array.findIndex (fun c -> c = ch)
    row, col

let startPos = find grid 'S'
let endPos   = find grid 'E'

grid[fst startPos][snd startPos] <- 'a'
grid[fst endPos][snd endPos] <- 'z'

type State = {
    Steps   : uint
    Position: Coord
}

let shortestPath 
    (startPos: Coord) 
    (hasReachedGoal: State -> bool)
    (isValidElevation: Coord -> Coord -> bool) 
    =
    let q = new System.Collections.Generic.Queue<State>()
    let mutable visited = Set.singleton startPos
    {
        Steps    = 0u
        Position = startPos
    }
    |> q.Enqueue

    let rec loop (): uint =
        let state = q.Dequeue()
        //printfn $"Position: %A{state.Position}, steps: %d{state.Steps}"

        if hasReachedGoal state then
            state.Steps
        else
            let validNext (row, col as next) =
                // inside grid
                row >= 0 && row < grid.Length && col >= 0 && col < grid[0].Length &&

                // not visited
                (not <| visited.Contains(next)) && 
        
                // below or at max elevation upwards
                isValidElevation state.Position next

            let nextState (coord: Coord) = 
                { state with
                    Steps = state.Steps + 1u
                    Position = coord }

            let enqueueAndVisit x =
                q.Enqueue(x)
                visited <- visited.Add(x.Position)

            let r, c = state.Position
            [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]
            |> List.filter validNext
            |> List.map nextState
            |> List.iter enqueueAndVisit
        
            loop ()
 
    loop ()
        
let gridAt coord = grid[fst coord][snd coord]

shortestPath 
    startPos 
    (fun state -> state.Position = endPos)
    (fun pos nextPos -> (int <| gridAt(nextPos)) - ((int <| gridAt(pos))) <= 1)
|> printfn "Part 1: %d"

shortestPath
    endPos 
    (fun state -> grid[fst state.Position][snd state.Position] = 'a')
    (fun pos prevPos -> (int <| gridAt(pos)) - ((int <| gridAt(prevPos))) <= 1)
|> printfn "Part 2: %d"