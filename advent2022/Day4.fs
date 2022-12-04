module Day4

let parse =
    let set (a:int[]) = Set [a[0]..a[1]]
    let pair (a:'a[]) = (a[0],a[1])
    let line = splitOn ',' >> Array.map (splitOn '-' >> Array.map int >> set) >> pair
    
    List.map line

let check f (a,b) = f a b || f b a

let part1 = List.filter (check Set.isSubset) >> List.length

let part2 =
    let overlap l r = Set.intersect l r <> Set.empty     

    List.filter (check overlap)  >> List.length

let Solve xs = xs |> parse |> both part1 part2

