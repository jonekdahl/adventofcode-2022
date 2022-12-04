type Range = int * int

let parseRange (s: string): Range =
    let parts = s.Split "-"
    (int parts[0]), (int parts[1])

let parsePair (s: string) =
    let parts = s.Split ","
    (parseRange parts[0]), (parseRange parts[1])

let assignments =
    System.IO.File.ReadAllLines("input")
    |> Seq.map parsePair

let fullyContains (r1: Range) (r2: Range) =
    let start1, end1 = r1
    let start2, end2 = r2
    (start2 >= start1 && start2 <= end1) && (end2 >= start1 && end2 <= end1)

assignments
|> Seq.filter (fun (r1, r2) -> fullyContains r1 r2 || fullyContains r2 r1)
|> Seq.length
|> printfn "Part 1: %A"

let overlaps (r1: Range) (r2: Range) =
    let start1, end1 = r1
    let start2, end2 = r2
    fullyContains r1 r2 || fullyContains r2 r1  || (start2 >= start1 && start2 <= end1) || (end2 >= start1 && end2 <= end1)

assignments
|> Seq.filter (fun (r1, r2) -> overlaps r1 r2)
|> Seq.length
|> printfn "Part 2: %A"
