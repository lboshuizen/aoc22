open  System.IO

open Utils

let readInput (day:int)  =
    File.ReadAllText($"../../../inputs/day{day}.txt")
    |> splitOn '\n'
    |> List.ofArray

readInput 1
|> Day1.Solve
|> printf "%A"
