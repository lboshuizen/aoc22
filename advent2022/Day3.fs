module Day3

let priority = function
               | c when c >= 'a' && c <= 'z' -> (int c) - int('a') + 1
               | c -> (int c) - int('A') + 27

let parse (xs:string list)= xs |> List.map List.ofSeq

let findDuplicate = Seq.map Set >> Set.intersectMany >> Seq.head >> priority

let part1 = List.map (List.splitInto 2 >> findDuplicate) >> Seq.sum

let part2 = List.chunkBySize 3 >> List.map findDuplicate >> Seq.sum

let Solve = parse >> both part1 part2 