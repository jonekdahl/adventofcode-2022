let grid =
    System.IO.File.ReadAllLines("input.txt")
    |> Seq.map (fun s -> s |> Seq.map System.Globalization.CharUnicodeInfo.GetDigitValue)
    |> array2D

let isVisibleRow (tree: int) (other: int[]) =
    tree > (other |> Array.max)

let isVisible (grid: int[,]) (x, y) =
    let minIndex = 0
    let maxIndex = (grid |> Array2D.length1) - 1
    let tree = grid[x,y]
    if x = minIndex || x = maxIndex || y = minIndex || y = maxIndex then 
        true
    else
        seq {
            grid[x, ..y-1]  // top
            grid[x, y+1..]  // bottom
            grid[..x-1, y]  // left
            grid[x+1.., y]  // right
        }
        |> Seq.exists (isVisibleRow tree)

let coordinates (grid: int[,]) =
    let gridLength = grid |> Array2D.length1
    seq { 
        for y in 0..gridLength-1 do
            for x in 0..gridLength-1 do
                yield x, y
    }

coordinates grid
|> Seq.filter (isVisible grid)
|> Seq.length
|> printfn "Part 1: %d"

let viewingDistance (tree: int) (others: int[]) =
    let lower =
        others
        |> Seq.takeWhile (fun other -> other < tree)
        |> Seq.length
    min (lower + 1) (others.Length)

let scenicScore (grid: int[,]) (x, y): int =
    let tree = grid[x,y]

    seq {
        grid[x, ..y-1] |> Array.rev  // up
        grid[x, y+1..]               // down
        grid[..x-1, y] |> Array.rev  // left
        grid[x+1.., y]               // right
    }
    |> Seq.map (viewingDistance tree)
    |> Seq.fold (fun acc x -> acc * x) 1


let innerCoordinates (grid: int[,]) =
    let gridLength = grid |> Array2D.length1
    let maxIndex = gridLength - 1
    coordinates grid
    |> Seq.filter (fun (x, y) -> x <> 0 && x <> maxIndex && y <> 0 && y <> maxIndex)

innerCoordinates grid
|> Seq.map (scenicScore grid)
|> Seq.max
|> printfn "Part 2: %d"
