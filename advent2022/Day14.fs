module Day14

let (<*>) f (a,b) (x,y) = f a x,f b y

let dim s = Seq.maxBy fst s |> fst,Seq.maxBy snd s |> snd

let parse = List.map ((splitOnS "->" >> Array.map ((splitOn ',') >> fun a -> (int a[0],int a[1]) )) >> Array.pairwise >> Array.map line2D)
            >> Array.concat >> List.concat >> Set >> both id dim

let blocked s = flip Set.contains s
let inVoid (s,v) = Set.exists (snd >> (=) v) s
let isFull (s,_) = Set.contains (500,0) s

let rec fall p (s,v) =
    let move s p = List.map ((+) <*> p) [(0,1);(-1,1);(1,1)] |> Seq.tryFind (blocked s >> not)

    match move s p with
    | Some (x,y) -> if y=v then (Set.add (x,y) s,v)
                           else fall (x,y) (s,v)
    | None -> (Set.add p s,v)

let fillUntil cond (s,(_,y)) = until cond (fall (500,0)) (s,y+1) |> fst |> flip Set.difference s |> Set.count

let part1 = fillUntil inVoid >> (+) -1

let part2 =
    let addFloor (s,(x,y)) = line2D ((0,y+2),(2*x,y+2)) |> Set |> Set.union s |> fun s -> (s,(x,y+1))
    
    addFloor >> fillUntil isFull
                       
let Solve xs = xs |> parse |> both part1 part2
