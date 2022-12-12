module Day11

type Monkey = int64 list * (int64 -> int64) * int64 * int * int * int

let values (m:Map<'a,'b>) = m |> Map.values |> List.ofSeq
let keys (m:Map<'a,'b>) = m |> Map.keys |> List.ofSeq
let inline update  (k:'k) (fu:'v -> 'v) (m:Map<'k,'v>) = Map.change k (Option.map fu) m

let parse =
    let buildOp (a:string []) =
        let op = function
                 | "*" -> (*)
                 | "+" -> (+)

        let inline self f (a:int64) = f a a 

        match a[0],a[1] with
        | o,v when isDigit v[0] -> (op o) (int64 v)
        | o,_ -> (self (op o))
    
    let build (a:string[]) =
            let id = a[0] |> parseRegex "(\d+)" (fun a -> (int a[0]))
            let items = a[1] |> splitOn ':' |> fun a -> a[1] |> splitOn ',' |> Array.map int64 |> List.ofArray
            let op = a[2] |> parseRegex ".*(\W)\s(\w+)$" buildOp
            let div = a[3] |> parseRegex "(\d+)" (fun a -> (int64 a[0]))
            let onT = a[4] |> parseRegex "(\d+)" (fun a -> (int a[0]))
            let onF = a[5] |> parseRegex "(\d+)" (fun a -> (int a[0]))
            
            id, (items,op,div,onT,onF,0)
    
    List.chunkBySize 7 >> List.map (Array.ofList >> build) >> Map

let receive mm (mk,i) = update mk (fun (items,o,d,t,f,c) -> (items@[i],o,d,t,f,c)) mm
let clear mk = update mk (fun (items,o,d,t,f,c) -> ([],o,d,t,f,c+List.length items))

let monkey f (mm:Map<int,Monkey>) mk =
    let items,op,div,wT,wF,_ = mm[mk]
    let tf a = if a % div = 0L then (wT,a) else (wF,a)  
    let throwTo f m i = receive m (f i)
    let dest = op >> f >> tf

    items |> List.fold (throwTo dest) mm |> clear mk
    
let round f mm = List.fold (monkey f) mm (keys mm)
    
let result = values >> List.map (fun (_,_,_,_,_,c) -> int64 c) >> List.sortDescending >> List.take 2 >> List.reduce (*)    
    
let part1 = times 20 (round (flip (/) 3L) ) >>  result

let part2 mm =
    let lcmOfDiv = values >> List.map (fun (_,_,div,_,_,_) -> div) >> List.fold lcm 1L
    
    times 10000 (round (flip (%) (lcmOfDiv mm))) mm |> result

let Solve : Solver<int64> = parse >> both part1 part2
