open System.Diagnostics
open  System.IO
open FSharpPlus.Data
open FSharpPlus.GenericBuilders

let readInput (d:int)  =
    let p = Path.Combine(__SOURCE_DIRECTORY__,"inputs",$"day{d}.txt")
    File.ReadLines(p) |> List.ofSeq

let go f xs = monad { return f xs }        

readInput 12
|> fun xs -> State.run (go Day12.Solve xs) (Stopwatch.StartNew())
|> fun (r,s) -> (s.ElapsedMilliseconds,r)
||> printf "time: %dms\nresult:\n %A" 
