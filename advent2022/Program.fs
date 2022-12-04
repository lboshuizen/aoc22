open  System.IO

let readInput (day:int)  =
    let p = Path.Combine(__SOURCE_DIRECTORY__,"inputs",$"day{day}.txt")
    File.ReadLines(p) |> List.ofSeq

readInput 4
|> Day4.Solve
|> printf "%A"
