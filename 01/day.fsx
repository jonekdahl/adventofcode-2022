let elves =
    System.IO.File.ReadAllText("input").TrimEnd()
    |> fun text -> text.Split("\n\n")
    |> Seq.map (fun lines -> 
                    lines.Split("\n")
                    |> Seq.map int
                    |> Seq.sum)
    |> List.ofSeq

elves
|> List.max
|> printfn "Part 1: %d" 

elves
|> List.sortDescending
|> List.take 3
|> List.sum
|> printfn "Part 2: %d"
