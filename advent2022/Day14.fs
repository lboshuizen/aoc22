module Day14

let inline (<*>) f (a,b) (x,y) = f a x,f b y

let dim = both (Seq.maxBy fst >> fst) (Seq.maxBy snd >> snd)

let parse = List.map ((splitOnS "->" >> Array.map ((splitOn ',') >> fun a -> (int a[0],int a[1]) )) >> Array.pairwise >> Array.map line2D)
            >> Array.concat >> List.concat >> Set >> both id dim
let source = (500,0)
let blocked = flip Set.contains
let inVoid (s,v) = Set.exists (snd >> (=) v) s
let isFull = fst >> Set.contains source 

let rec fallFrom p (s,v) =
    let inline move s p = List.map ((+) <*> p) [(0,1);(-1,1);(1,1)] |> Seq.tryFind (blocked s >> not)

    match move s p with
    | None -> Set.add p s, v
    | Some (_,y) when y>v -> Set.add p s, v
    | Some (x,y) -> fallFrom (x,y) (s,v)

let fillUntil cond (s,(_,y)) =
    let diff = fst >> flip Set.difference s
    
    until cond (fallFrom source) (s,y+1) |> diff

let part1 = fillUntil inVoid >> Set.count >> minus 1

let part2 =
    let addFloor (s,(x,y)) = line2D ((0,y+2),(2*x,y+2)) |> Set |> Set.union s |> fun s -> (s,(x,y+1))
    
    addFloor >> fillUntil isFull >> Set.count
                       
let Solve : Solver<int> = parse >> both part1 part2 >> shouldBe 858 26845
