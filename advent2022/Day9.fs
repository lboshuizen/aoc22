module Day9

let inline (<!>) (f,g) (a,b) = (f a,g b)

let parse = List.map (splitOn ' ' >> fun [|l;r|] -> (l,int r) )

let track xs (h,t) = ((h,t),(t::xs))
let count = List.distinct >> List.length

let moveTail =
    let df a b = (+) (sign (a-b))
    let delta (x,y) (x',y') = (df x x'),(df y y')
    
    function
    | h,t when dist h t <= 2 -> (h,t)
    | h,t -> h,delta h t <!> t
                   
let move f ((h,t),xs) = moveTail (f <!> h,t) |> track xs 

let step s = function
             | "U",n -> times n (move (id,inc)) s
             | "D",n -> times n (move (id,dec)) s
             | "R",n -> times n (move (inc,id)) s
             | "L",n -> times n (move (dec,id)) s
             | _ -> failwith "wtf"
                                 
let trail = List.fold step (((0,0),(0,0)),[])>> snd >> List.rev                  
let knots = List.fold (fun ((_,t),xs) h -> moveTail (h,t) |> track xs) (((0,0),(0,0)),[]) >> snd >> List.rev


let part1 = trail >> count

let part2  = trail >> times 8 knots >> count

let Solve : Solver<int> = parse >> both part1 part2
