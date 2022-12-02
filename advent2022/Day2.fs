module Day2

let trans s = match s with
              | "A" | "X" -> 1
              | "B" | "Y" -> 2
              | "C" | "Z" -> 3

let parse = List.map (splitOn ' ' >> fun a -> (trans a[0], trans a[1]))

let lose x = if x-1=0 then 3 else (x-1)
let win x =  if x+1=4 then 1 else (x+1)

let game f s h  = s + (f >> uncurry (+)) h

let part1 =
    let rules h = match h with
                  | (r,l) when l = lose r -> (0,l)
                  | (l,r) when l = r -> (3,r)
                  | (_,r) -> (6,r) 
    
    List.fold (game rules) 0

let part2 = 
    let rules h = match h with
                  | (r,1) -> (0, lose r)
                  | (r,2) -> (3, r)
                  | (r,3) -> (6, win r)
    
    List.fold (game rules) 0

// string list -> int * int
let Solve = parse >> both part1 part2 

