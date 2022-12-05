open  System.IO

let readInput (day:int)  =
    let p = Path.Combine(__SOURCE_DIRECTORY__,"inputs",$"day{day}.txt")
    File.ReadLines(p) |> List.ofSeq

readInput 5
|> Day5.Solve
|> printf "%A"
