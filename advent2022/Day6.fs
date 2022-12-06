module Day6

let parse = List.head >> List.ofSeq

let allDistinct xs = List.distinct xs = xs

let firstMarker n = List.windowed n >> List.findIndex allDistinct >> (+) n

let part1 = firstMarker 4
let part2 = firstMarker 14

let Solve : Solver<int> = parse >> both part1 part2
