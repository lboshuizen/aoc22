module Day15

let dim = Seq.map (fun ((x,_),d) -> (x-d),(x+d)) >> both (Seq.minBy fst >> fst) (Seq.maxBy snd >> snd)

let parse = List.map (parseRegex "(-?\d+).*=(-?\d+).*=(-?\d+).*=(-?\d+)$" (fun a -> (int a[0],int a[1]),(int a[2],int a[3])) >> fun (p1,p2) -> (p1,manhattan p1 p2) )
            >> both id dim

let exclude r (a,b) =
    match r with
    | x,x' when x' < a || x > b -> [(a,b)]
    | x,x' when x <= a && x' >= b -> []
    | x,x' when x > a && x' < b -> [(a,x-1);(x'+1,b)]
    | _,x' when x' < b -> [(x'+1,b)]
    | x,_ when x > a -> [(a,x-1)]
    | _ -> failwith "missed case"
    
let notCovered (mn,mx) row =
    let cover row ((x,y),d) = let dy = d-abs(row-y)
                              (x-dy,x+dy)
    let remove range = List.map (exclude range) >> List.concat
    
    List.filter (fun ((_,y),d) -> (abs (row-y)) <= d) >> List.map (cover row)
    >> List.fold (flip remove) [(mn,mx)]
    >> fun xs -> (row,xs)
    
let part1 (sensors,(mn,mx)) =
    sensors |> (notCovered (mn,mx)) 2_000_000
    |> snd |> List.pairwise |> List.map (fun ((_,a),(b,_)) -> b-a) |> List.sum

let part2 (sensors,_) =
    let unwrap (y,xs) = xs |> List.head |> (fst >> int64), int64 y
    let tune = List.head >> unwrap >> fun (x,y) -> x * 4_000_000L + y
    [0..4_000_000] |> List.map (flip (notCovered (0,4_000_000)) sensors) |> List.filter (snd >> (<>) []) |> tune

let Solve  = parse >> both part1 part2 >> shouldBe 4_985_195 11_583_882_601_918L
