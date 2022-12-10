module Day10

type Op =
    | Noop
    | Add of int

let parse = List.map (function
                     | (l:string) when l.StartsWith "add" -> l |> splitOn ' ' |> fun a -> Add (int a[1])
                     | _ -> Noop)

let cycle (x,c) = function
                  | Noop -> (x,c @ [x])
                  | Add n -> (x+n,c @ [x;x+n])
                  
let run = List.fold cycle (1,[1]) >> snd 
let part1 =
    let pick (xs:int list) = List.map (fun c -> c*xs[c-1]) [20..40..220]
    run >> pick >> List.sum

let part2 =
    let draw i x = if i >= (x-1) && i <= (x+1) then '#' else '.'
    run >> List.chunkBySize 40 >> List.map (List.mapi draw ) >> List.map toString

let Solve (xs:string list) = xs |> parse |> both part1 part2
