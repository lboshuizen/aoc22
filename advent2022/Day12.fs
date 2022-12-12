module Day12

open FSharp.Collections.ParallelSeq
open AStar

let map (a,b) f (a',b') = (f a a',f b b')

let normalize (p,h) = match h with
                      | 'S' -> (p,0)
                      | 'E' -> (p,25)
                      |  c ->  (p,(int c)-(int 'a'))

let parse xs = let g = toGrid2d xs
               let get s v = List.filter (fun (_,c) -> c = s) >> List.head >> mapSnd (fun _ -> v) 
               (g |> List.map normalize, g |> get 'S' 0, g |> get 'E' 25)

let cfg g =
    let around p = Seq.map (map p (+)) [(-1,0);(1,0);(0,1);(0,-1)]
    {
        neighbours = fun (p,h) -> g |> Seq.filter (fun (p',h') -> h'-h <= 1 && Seq.contains p' (around p))
        gCost = fun _ _ -> 1.0;
        fCost = fun (_,h) (_,h') -> abs (h-h') |> float
        maxIterations = Some 9999
    }

let aStar c e s = search s e c

let part1 (g,s,e) = aStar (cfg g) e s |> Option.get |> Seq.length |> (+) (-1)
    
let part2 (g,_,e) =
    g |> List.filter (snd >> (=) 0) |> PSeq.map (aStar (cfg g) e) 
      |> Seq.choose id |> Seq.map Seq.length |> Seq.min |> (+) (-1)

let Solve : Solver<int> = parse >> both part1 part2
