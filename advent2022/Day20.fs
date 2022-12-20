module Day20

let parse (xs:string list) = xs |> List.map int64

let (%%) a b = (a % b + b) % b

let pos l = function
            | 0L -> l 
            | n ->  n %% (int64 l) |> int

let move p p' a = List.removeAt p >> List.insertAt p' a 

let number i s = List.findIndex (fst >> (=) i) s |> fun p -> p,s[p] 

let mixer l s i = let p,(i,n) = number i s
                  let p' = pos (l-1) (int64 p+n)
                  move p p' (i,n) s
let mix l = flip (List.fold (mixer l)) [0..(l-1)]

let decrypt n (xs,l) = times n (mix l) (List.indexed xs) |> List.map snd,l

let pick ix (xs,l) = let take o n = (o + n) % l
                     List.map (take (List.findIndex ((=)0L) xs) ) ix |> List.fold (fun s i -> xs[i]::s) []   
    
let sumOf ix = pick ix >> List.sum
    
let part1 = both id List.length >> decrypt 1 >> sumOf [1000;2000;3000] 

let part2 = let applyKey = List.map ((*) 811589153L)

            both applyKey List.length >> decrypt 10 >> sumOf [1000;2000;3000]
        
let Solve : Solver<int64> = parse >> both part1 part2 >> shouldBe 8372L 7865110481723L
