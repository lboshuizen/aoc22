open  System.IO

let readInput (day:int)  =
    let p = Path.Combine(__SOURCE_DIRECTORY__,"inputs",$"day{day}.txt")
    File.ReadLines(p) |> List.ofSeq

readInput 3
|> Day3.Solve
|> printf "%A"
