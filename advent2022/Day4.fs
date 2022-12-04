module Day4

let parse =
    let line = parseRegex "(\d+)-(\d+),(\d+)-(\d+)" (fun a -> (Set [int a[0]..int a[1]],Set [int a[2]..int a[3]]))
    List.map line

let check f (a,b) = f a b || f b a

let part1 = List.filter (check Set.isSubset) >> List.length

let part2 =
    let overlap l r = Set.intersect l r <> Set.empty     

    List.filter (check overlap)  >> List.length
let Solve xs = xs |> parse |> both part1 part2

