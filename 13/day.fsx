#r "nuget: FParsec"


type PacketData =
    | PacketList of PacketData list
    | Int of int


module PacketData =
    open FParsec
    
    let pint = pint32 |>> fun x -> Int x
    let pLeftBracket = pchar '['
    let pRightBracket = pchar ']'
    let pdata, pdataRef = createParserForwardedToRef<PacketData, unit>()
    let pListItem = pint <|> pdata
    let plistValues =  sepBy pListItem (pchar ',')
    let plist = between pLeftBracket pRightBracket plistValues |>> PacketList
    pdataRef.Value <- plist

    let parse (s: string): PacketData =
        match run pdata s with
        | Success (packetData, _, _) -> packetData
        | Failure (errorMsg, _, _) -> failwith errorMsg

let packets =
    System.IO.File.ReadAllText("input.txt").Split("\n\n")
    |> Seq.map (fun s -> 
        match s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) with
        | [| p1; p2 |] -> PacketData.parse p1, PacketData.parse p2
        | other -> failwithf "Unexpected input: %A" other)

let rec compare (p1: PacketData) (p2: PacketData): int32 =
    match p1, p2 with
    | Int left, Int right ->
        if left < right then
            -1
        else if right < left then
            1
        else
            0
    | PacketList data1, PacketList data2 ->
        match data1, data2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | h1 :: t1,  h2 :: t2 ->
            match compare h1 h2 with
            | 0 -> compare (PacketList t1) (PacketList t2)
            | other -> other
    | PacketList _, Int _ ->
        compare p1 (PacketList [p2])
    | Int _, PacketList _ ->
        compare (PacketList [p1]) p2

packets
|> Seq.mapi (fun idx pd -> idx + 1, pd)
|> Seq.filter (fun (_idx, (p1, p2)) -> (compare p1 p2) = -1)
|> Seq.sumBy fst
|> printfn "Part 1: %A"

let divider1 = PacketList [PacketList [Int 2]]
let divider2 = PacketList [PacketList [Int 6]]

let decoderKey (ps: PacketData list) =
    let idx1 = ps |> List.findIndex (fun p -> p = divider1)
    let idx2 = ps |> List.findIndex (fun p -> p = divider2)
    (idx1 + 1) * (idx2 + 1)

packets
|> Seq.map (fun (p1, p2) -> seq { p1; p2 })
|> Seq.concat
|> Seq.append (seq { divider1 ; divider2 })
|> Seq.sortWith compare
|> List.ofSeq
|> decoderKey
|> printfn "Part 2: %A"
