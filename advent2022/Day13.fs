module Day13

type NumList =
    | Num of int
    | List of NumList list

let parse = splitWhen ((=)"")

let atoi = Seq.fold (fun n c -> n*10 + asInt c) 0

let parseInt i s =
    let xs = s |> Seq.skip i |> Seq.takeWhile isDigit
    (i+Seq.length xs, atoi xs)

let numList (s:string) =

    let rec go ptr acc =
        match s.[ptr] with
        | '[' -> let p, xs = go (ptr+1) []
                 go p (acc @ [List xs])
        | ']' -> (ptr+1, acc)
        | ',' -> go (ptr+1) acc
        | _ ->   let p, n = parseInt ptr s
                 go p (acc @ [Num n])
                 
    go 1 [] |> snd |> List

let rec compare a b =
    
    let compareNum a b = sign(a-b) 
    
    let compareLst l r = Seq.zip l r |>  Seq.map (uncurry compare) |> Seq.tryFind ((<>)0)
    
    match a, b with
    | Num l, Num r -> compareNum l r
    | Num l, List _ -> compare (List [Num l]) b
    | List _, Num r -> compare a (List [Num r])
    | List l, List r -> match compareLst l r with
                        | None -> compareNum l.Length r.Length
                        | Some c -> c

let indexOf p = List.indexed >> List.filter (snd >> p) >> List.map (fst >> (+)1)

let part1 =
    let compared = List.map numList >> List.pairwise >> List.head >> uncurry compare
    
    List.map compared >> indexOf ((=) -1) >> List.sum

let part2 =
    let dividers = ["[[2]]";"[[6]]"] |> List.map numList
    let isDivider = flip List.contains dividers 
    
    List.concat >> List.map numList >> (@) dividers >> List.sortWith compare >> indexOf isDivider >> List.reduce (*)

let Solve : Solver<int> = parse >> both part1 part2
