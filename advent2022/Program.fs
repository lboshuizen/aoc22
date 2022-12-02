open  System.IO

let readInput (day:int)  =
    File.ReadLines($"../../../inputs/day{day}.txt")
    |> List.ofSeq

readInput 2
|> Day2.Solve
|> printf "%A"
