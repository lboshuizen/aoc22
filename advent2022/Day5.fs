module Day5

let parse xs =
    let [s;o]  = splitWhen ((=)"") xs
    
    let stacks = s |> List.rev |> List.map (List.ofSeq) |> List.transpose
                   |> List.choose (function
                                   | h::t  when isDigit h -> t |> List.filter ((<>) ' ' ) |> List.rev |> Some
                                   | _ -> None)
                   |> Array.ofList
    
    let orders = o |> List.map (parseRegex "\D+(\d+)\D+(\d+)\D+(\d)" (fun a -> (int a[0], (int a[1])-1, (int a[2])-1)))
    
    (stacks,orders)

let move (f:'a list->'a list) (st:'a list[]) (n,src,dest) =
    let (h,t) = List.splitAt n st[src] |> mapFst f
    st |> Array.updateAt dest (h @ st[dest]) |> Array.updateAt src t
    
let move9000 = move List.rev
let move9001 = move id

let part1 st = foldl move9000 st >> Seq.map List.head >> toString

let part2 st = foldl move9001 st >> Seq.map List.head >> toString

let Solve = parse >> both (uncurry part1) (uncurry part2)

