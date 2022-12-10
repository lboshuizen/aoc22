module DayXX

let parse xs = xs

let part1 = id

let part2 _ = None

// string list -> ('a,'a)
let Solve xs = xs |> parse |> both part1 part2
