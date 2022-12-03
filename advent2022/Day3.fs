module Day3

let priority = function
               | c when c >= 'a' && c <= 'z' -> (int c) - int('a') + 1
               | c -> (int c) - int('A') + 27

let scan (xs:string) =
    let (l,r) = xs |> List.ofSeq |> List.splitAt (xs.Length / 2)
    List.find (flip List.contains r) l |> priority

let badge = function
            | h::t -> let inOthers (b:char) = t |> List.forall (fun (s:string) -> s.Contains(b))
                      h |> Seq.find inOthers |> priority
            | _ -> failwith "oops"

let part1 = List.map scan >> List.sum

let part2 = List.chunkBySize 3 >> List.map badge  >> List.sum

let Solve = both part1 part2 