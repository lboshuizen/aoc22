module Day15

open FSharp.Collections.ParallelSeq

let dim = Seq.map (fun ((x,_),d) -> (x-d),(x+d)) >> both (Seq.minBy fst >> fst) (Seq.maxBy snd >> snd)

let parse = List.map (parseRegex "(-?\d+).*=(-?\d+).*=(-?\d+).*=(-?\d+)$"
                     (fun a -> (int a[0],int a[1]),(int a[2],int a[3])) >> fun (s,b) -> (s,manhattan s b) )
            >> both id dim

let exclude r (a,b) =
    match r with
    | x,x' when x' < a || x > b -> [(a,b)]
    | x,x' when x <= a && x' >= b -> []
    | x,x' when x > a && x' < b -> [(a,x-1);(x'+1,b)]
    | _,x' when x' < b -> [(x'+1,b)]
    | x,_  when x > a ->  [(a,x-1)]
    | _ -> failwith "missed case"
    
let notCovered clip row =
    let cover ((x,y),d) = let dy = d-abs(row-y)
                          (x-dy,x+dy)
    let remove range = List.map (exclude range) >> List.concat
    let sensorCoversRow ((_,y),d) = (abs (row-y)) <= d
    
    List.filter sensorCoversRow >> List.map cover
    >> List.fold (flip remove) [clip]
    >> function
       | [] -> None
       | xs -> Some (row,xs) 
    
let part1 (sensors,clip) =
    sensors |> (notCovered clip) 2_000_000
    |> Option.get |> snd |> List.pairwise |> List.map (fun ((_,a),(b,_)) -> b-a) |> List.sum

let part2 (sensors,_) =
    let unwrap (y,xs) = xs |> List.head |> (fst >> int64), int64 y
    let tune = Seq.choose id >> Seq.head >> unwrap
    
    let x,y = [0..4_000_000] |> PSeq.map (flip (notCovered (0, 4_000_000)) sensors) |> tune
    x * 4_000_000L + y

let Solve = parse >> both part1 part2 >> shouldBe 4_985_195 11_583_882_601_918L
