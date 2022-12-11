let input =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.head

let findMarker (windowSize: int) (input: string) =
    input
    |> Seq.windowed windowSize
    |> Seq.findIndex (fun chars -> (Set chars).Count = windowSize)
    |> fun windowIdx -> windowIdx + windowSize

input
|> findMarker 4
|> printfn "Part 1: %d"

input
|> findMarker 14
|> printfn "Part 2: %d"
