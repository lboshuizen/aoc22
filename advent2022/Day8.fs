module Day8
let parse = toGrid2d >> List.map (mapSnd asInt)

let dim = List.rev >> List.head >> fst

let visible pred dir grd idx =
      let height = List.head >> snd >> fst

      let line = grd |> List.filter (pred idx) |> dir
      line |> List.mapFold (fun mh (p,(h,v)) -> (p,(h, v || h > mh )),(max mh h) ) (height line) |> fst

let scan f p d m xs = List.map (f p d xs) [0..m] |> List.concat
let scanBiDir pred mx = scan visible pred id mx >> scan visible pred List.rev mx

let part1 grd =
    let (mx,my) = dim grd
    let borderLength = (2*((mx+1)+(my-1)))
    let notOnBorder (x,y) = x<>0 && y<>0 && x<>mx && y<>my
    let onCol c ((x,_),_) = x = c
    let onRow c ((_,y),_) = y = c

    grd |> List.map (fun (p,h) -> (p,(h,false)))
        |> (scanBiDir onCol mx >> scanBiDir onRow my)
        |> List.filter (fun (p,(_,v)) -> v = true && notOnBorder p)
        |> List.length |> (+) borderLength

let scenic (mx,my) (m:Map<int * int,int>) ((x,y),h) =
    let lineOfSight xs = match List.tryFindIndex (fun h' -> h' >= h) xs with
                         | None -> List.length xs
                         | Some i -> i+1

    [[for x' in 0..x-1 do (x',y)] |> List.rev
     [for x' in (x+1)..mx do (x',y)]
     [for y' in 0..y-1 do (x,y')] |> List.rev
     [for y' in y+1..my do (x,y')]] 
    |> List.map (List.choose (flip Map.tryFind m) >> lineOfSight)
    |> List.reduce (*)
              
let part2 grd = grd |> List.map (scenic (dim grd) (Map grd)) |> List.max

let Solve :Solver<int> = parse >> both part1 part2

