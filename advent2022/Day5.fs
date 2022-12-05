module Day5

let parse xs =
    let s::o::_  = splitWhen ((=)"") xs
    
    let stacks = s |> List.rev |> List.tail |> List.map List.ofSeq |> List.transpose
                   |> List.mapi (fun i a -> (i,a))
                   |> List.filter (fun (i,_) -> (i-1) % 4 = 0) |> List.map (snd >> List.filter ((<>)' ') >> List.rev)
                   |> Array.ofList
    
    let orders = o |> List.map (parseRegex "\D+(\d+)\D+(\d+)\D+(\d)" (fun a -> (int a[0], (int a[1])-1, (int a[2])-1)))
    
    (stacks,orders)

let move (f:'a list->'a list) (st:'a list[]) (n,from,t) =
    let r = List.take n st[from] |> f
    st |> Array.updateAt t (r @ st[t]) |> Array.updateAt from (List.skip n st[from])
    
let move9000 = move List.rev
let move9001 = move id

let part1 st = List.fold move9000 st >> Seq.map List.head >> toString

let part2 st = List.fold move9001 st >> Seq.map List.head >> toString

let Solve xs = xs |> parse |> both (uncurry part1) (uncurry part2)

