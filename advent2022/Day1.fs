module Day1

let parse = splitWhen ((=)"") >> List.map (List.map int)

let part1 = List.map List.sum >> List.max

let part2 = List.map List.sum >> List.sortDescending >> List.take 3 >> List.sum

let Solve = parse >> both part1 part2 

