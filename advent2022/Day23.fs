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

let area s = let (x,x'),(y,y') = s |> both (Seq.map fst >> both Seq.min Seq.max) (Seq.map snd >> both Seq.min Seq.max)
             (x'-x+1)*(y'-y+1)

let onRow y = Seq.exists (snd >> (=) y) >> not
let onCol x = Seq.exists (fst >> (=) x) >> not

let look = [(pred,pred);(id,pred);(succ,pred);(pred,id);(succ,id);(pred,succ);(id,succ);(succ,succ)]
let props = [snd >> pred >> onRow;snd >> succ >> onRow;fst >> pred >>onCol;fst >> succ >> onCol] // N,S,W,E
let moves = [(<!>)(id,pred);(<!>)(id,succ);(<!>)(pred,id);(<!>)(succ,id)]

let propose ((g,ds,mv):State) p =
    let around = List.map (flip (<!>) p) look |> List.filter (flip Set.contains g)
    let prop (x,y) xs = ds |> Seq.map ((<*>) (x,y) >> (<*>) xs) |> Seq.tryFindIndex ((=) true)
    match around with
    | [] -> None
    | xs -> prop p xs |> Option.map (fun d -> p,p <*> mv[d])

let duplicates = PSeq.groupBy snd >> PSeq.filter (snd >> Seq.length >> flip (>) 1) >> Seq.map fst >> Set

let notCollided xs = xs |> PSeq.filter (snd >> flip Set.contains (duplicates xs) >> not)

let round (g,ds,mv) =
    let replace (src,dst) = Set.remove src >> Set.add dst
    let next = g |> PSeq.map (propose (g,ds,mv)) |> PSeq.choose id
               |> notCollided |> PSeq.fold (flip replace) g
             
    next, rotate ds, rotate mv                 

let part1 s = times 10 round (s,props,moves) |> fst3 |> both id area |> fun (s,a) -> a - Set.count s

let part2 s = let next ((_,i),(s,ds,mv)) = (s,succ i),round (s,ds,mv)
              until (fun ((s,_),(s',_,_)) -> s'=s) next ((Set.empty,0),(s,props,moves)) |> fst |> snd                         

let Solve : Solver<int> = parse >> both part1 part2 >> shouldBe 3996 908
