let rucksacks =
    System.IO.File.ReadAllLines("input")

let splitByCompartment (r: string) =
    let cLength = r.Length / 2
    seq { 
        r[0..cLength-1]
        r[cLength..r.Length-1]
    }

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
|> Seq.map splitByCompartment
|> Seq.map findDuplicatedItem
|> Seq.sumBy priority
|> printfn "Part 1: %A"

rucksacks
|> Seq.chunkBySize 3
|> Seq.map findDuplicatedItem
|> Seq.sumBy priority
|> printfn "Part 2: %A"
