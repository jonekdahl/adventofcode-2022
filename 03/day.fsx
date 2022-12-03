let rucksacks =
    System.IO.File.ReadAllLines("input")

let splitInTwo (r: string) =
    r
    |> Seq.splitInto 2
    |> Seq.map System.String.Concat

let findDuplicatedItem (rs: string seq) =
    rs
    |> Seq.map Set
    |> Set.intersectMany
    |> Set.minElement

let priority (ch: char) = 
    if ch >= 'a' && ch <= 'z' then
        (int ch) - (int 'a') + 1
    else
        (int ch) - (int 'A') + 27

rucksacks
|> Seq.map splitInTwo
|> Seq.map findDuplicatedItem
|> Seq.sumBy priority
|> printfn "Part 1: %A"

rucksacks
|> Seq.chunkBySize 3
|> Seq.map findDuplicatedItem
|> Seq.sumBy priority
|> printfn "Part 2: %A"
