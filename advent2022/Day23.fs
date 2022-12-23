module Day23

open FSharp.Collections.ParallelSeq

let (<!>) (f,g) (x,y) = (f x, g y)
let (<*>) a f = f a
let fst3 (a,_,_) = a
let rotate = function
             | [] -> []
             | [x] -> [x]
             | x::xs -> xs @ [x]

type xy = int * int
type fProps = (xy -> xy list -> bool) list
type fProj = (xy -> xy) list
type State = Set<xy> * fProps * fProj

let parse = List.map List.ofSeq >> toGrid2d >> List.filter (snd >> (=) '#') >> List.map fst >> Set

let onRow y = Seq.exists (snd >> (=) y) >> not
let onCol x = Seq.exists (fst >> (=) x) >> not
let look = [(pred,pred);(id,pred);(succ,pred);(pred,id);(succ,id);(pred,succ);(id,succ);(succ,succ)]
let directions = [snd >> pred >> onRow;snd >> succ >> onRow;fst >> pred >>onCol;fst >> succ >> onCol]
let moves = [(<!>) (id,pred);(<!>) (id,succ);(<!>) (pred,id);(<!>) (succ,id)]

let propose (g,d,_) p = let around = List.map (flip (<!>) p) look |> List.filter (flip  Set.contains g)
                        let prop (x,y) xs = d |> List.map ((<*>) (x,y) >> (<*>) xs) |> Seq.tryFindIndex ((=) true)
                        match around with
                        | [] -> None
                        | xs -> prop p xs

let project ((g,ds,mv):State) p = propose (g,ds,mv) p |> Option.map (fun d -> p <*> mv[d]) |> fun p' -> p,p'

let duplicates = PSeq.groupBy snd >> Seq.filter (snd >> Seq.length >> flip (>) 1) >> Seq.map fst >> Set

let collided xs = xs |> List.partition (snd >> flip Set.contains (duplicates xs))

let round (g,ds,mv) = let mvrs,stay = g |> PSeq.map (project (g,ds,mv)) |> List.ofSeq |> List.partition (snd >> Option.isSome)
                      let col,ok = mvrs |> List.map (mapSnd Option.get) |> collided
                      (List.map fst stay) @ (List.map fst col) @ (List.map snd ok) |> Set, rotate ds, rotate mv

let area s = let xs = Set.toList s
             let x,x' = xs |> List.map fst |> both List.min List.max
             let y,y' = xs |> List.map snd |> both List.min List.max
             (x'-x+1)*(y'-y+1)

let part1 s = times 10 round (s,directions,moves) |> fst3 |> both id (area) |> fun (s,b) -> b - Set.count s

let part2 s = let next ((_,i),(s,ds,mv)) = (s,succ i),round (s,ds,mv)
              until (fun ((s,_),(s',_,_)) -> s'=s) next ((Set.empty,0),(s,directions,moves)) |> fst |> snd                         

let Solve : Solver<int> = parse >> both part1 part2 >> shouldBe 3996 908
