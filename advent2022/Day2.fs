module Day2

let trans = function 
           | "A" | "X" -> 1
           | "B" | "Y" -> 2
           | "C" | "Z" -> 3

let parse = List.map (splitOn ' ' >> fun a -> (trans a[0], trans a[1]))

let lose x = if x-1=0 then 3 else (x-1)
let win x =  if x+1=4 then 1 else (x+1)

let game f s h  = s + (f >> uncurry (+)) h

let part1 =
    let rules = function
                | (l,r) when r = lose l -> (0,r)
                | (l,r) when l = r -> (3,r)
                | (_,r) -> (6,r) 
    
    List.fold (game rules) 0

let part2 = 
    let rules = function
                | (l,1) -> (0, lose l)
                | (l,2) -> (3, l)
                | (l,_) -> (6, win l)
    
    List.fold (game rules) 0

// string list -> int * int
let Solve = parse >> both part1 part2 

