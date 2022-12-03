type Rucksack = string * string

let rucksacks =
    System.IO.File.ReadAllLines("input")

let splitByCompartment (r: string) =
    let cLength = r.Length / 2
    r[0..cLength-1], r[cLength..r.Length-1]

type System.String with
    member this.ContainsChar (ch: char) = this.IndexOf(ch) >= 0

let findDuplicatedItem2 (r1: string, r2: string) =
    seq { for ch in r1 do yield ch }
    |> Seq.find (fun (item: char) -> r2.ContainsChar(item))

let priority (ch: char) = 
    if ch >= 'a' && ch <= 'z' then
        (int ch) - (int 'a') + 1
    else
        (int ch) - (int 'A') + 27

rucksacks
|> Seq.map splitByCompartment
|> Seq.map findDuplicatedItem2
|> Seq.sumBy priority
|> printfn "Part 1: %A"

let groupByThree (rucksacks: string seq) =
    rucksacks
    |> Seq.chunkBySize 3
    |> Seq.map (fun rs -> rs[0], rs[1], rs[2])

let findDuplicatedItem3 (r1: string, r2: string, r3: string) =
    seq { for ch in r1 do yield ch }
    |> Seq.find (fun (item: char) -> r2.ContainsChar(item) && r3.ContainsChar(item))

rucksacks
|> groupByThree
|> Seq.map findDuplicatedItem3
|> Seq.sumBy priority
|> printfn "Part 2: %A"
