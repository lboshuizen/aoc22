module Day17

let (@@) xs s = Seq.fold (flip Set.add) s xs
let rotate = function
             | [] -> []
             | [x] -> [x]
             | x::xs -> xs @ [x]
let pop<'a> : 'a list -> 'a * 'a list = both List.head rotate

let rocks = [ [(2,0);(3,0);(4,0);(5,0)]
              [(3,0);(2,1);(3,1);(4,1);(3,2)]
              [(4,0);(4,1);(4,2);(2,0);(3,0)]
              [(2,0);(2,1);(2,2);(2,3)]
              [(2,0);(3,0);(2,1);(3,1)] ]

let floor = [(0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0)] |> Set
                   
let moveH blocked f = function
                      | xs when blocked xs -> xs
                      | xs -> List.map f xs 

let parse = List.head >> List.ofSeq 

let move w (gs,r) =
    let blockedL r = Seq.exists (fun (x,y) ->  x=0 || Set.contains (x-1,y) w) r
    let blockedR r = Seq.exists (fun (x,y) ->  x=6 || Set.contains (x+1,y) w) r
    let left =  moveH blockedL (mapFst pred)
    let right = moveH blockedR (mapFst succ)
    let down = List.map (mapSnd pred)
                    
    let g,gs' = pop gs
    let r' = match g with
             | '<' ->  left r
             | '>' ->  right r
             | _ ->    r
    (gs',down r')

let world (w,bs,gs) =
    let blockedDown w (_,r) = Seq.exists (fun (x,y) -> Set.contains (x,y) w) r
    let rock y = pop >> mapFst (List.map (mapSnd ((+) y)))
    let settle  = List.map (mapSnd succ)
                      
    let top     = w |> Seq.maxBy snd |> snd
    let b,bs'  = rock (top+(3+1)) bs
    let gs',b' = until (blockedDown w) (move w) (gs,b)
    ((settle b') @@ w, bs', gs')

let height n gs = times n world (floor,rocks,gs) |> fun (w,_,_) -> w |> Seq.maxBy snd |> snd

let part1 = height 2022

let part2 gs = // I figured out that my input repeats after 1690 cycles
               // can't publish the code, as it was trial by error in 2 interactive sessions.
               // closing the wrong one instantly on "Eureka" 
               let repeats = 1690
               let target = 1000000000000L 
               let h = height repeats gs
               let hAfterRepeats = (target / (int64 repeats)) * (int64 h)
               let rem = target % (int64 repeats) |> int
               let hRemaining = height rem gs
               hAfterRepeats + (int64 hRemaining)

let Solve (xs:string list) = xs |> parse |> both part1 part2 |> shouldBe 3055 1507692307690L
