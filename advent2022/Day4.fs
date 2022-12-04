module Day4

let parse =
    let pair (h::t::_) = (h,t)
    let line = splitOn ',' >> List.map (splitOn '-' >> List.map int >> pair) >> pair
    
    List.map line

let check f = function
              | l,r when f l r -> true
              | l,r when f r l -> true
              | _ -> false

let part1 =
    let contains (x,y) (x',y') = x >= x' && y <= y'
            
    List.filter (check contains) >> List.length

let part2 =
    let overlap (_,y) (x',y') = y >= x' && y <= y'    

    List.filter (check overlap)  >> List.length

let Solve xs = xs |> parse |> both part1 part2

