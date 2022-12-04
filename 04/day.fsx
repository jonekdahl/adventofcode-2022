type Range = Set<int>

let parseRange (s: string): Range =
    let parts = s.Split "-"
    [int parts[0] .. (int parts[1])]
    |> Set

let parsePair (s: string) =
    let parts = s.Split ","
    (parseRange parts[0]), (parseRange parts[1])

let assignments =
    System.IO.File.ReadAllLines("input")
    |> Seq.map parsePair

let fullyContainsEachOther (r1: Range) (r2: Range) =
    Set.isSubset r1 r2 || Set.isSubset r2 r1

assignments
|> Seq.filter (fun (r1, r2) -> fullyContainsEachOther r1 r2)
|> Seq.length
|> printfn "Part 1: %A"

let overlapsEachOther (r1: Range) (r2: Range) =
    Set.intersect r1 r2
    |> Set.isEmpty
    |> not

assignments
|> Seq.filter (fun (r1, r2) -> overlapsEachOther r1 r2)
|> Seq.length
|> printfn "Part 2: %A"
