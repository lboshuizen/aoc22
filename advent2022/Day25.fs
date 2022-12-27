module Day25

let decode = function
             | d when isDigit d -> asInt d |> int64
             | '-' -> -1L
             | '=' -> -2L

let encode : int -> char = function
                           | -2 -> '='
                           | -1 -> '-'
                           | n -> 48 + n |> char

let parse = List.map (List.ofSeq >> List.map decode)

let toDec = List.fold (fun s d -> s * 5L + d) 0L

let fact n = match n / 5L, (n % 5L) |> int with
             | d,r when r < 3 -> d,r
             | d,r ->            d,r-5

let snafu =
    let rec go xs = function
                    | 0L -> xs
                    | n -> match fact n with
                           | d,r when r < 0 -> go (r::xs) (d + 1L) 
                           | d,r ->            go (r::xs) d
                             
    go [] >> List.map encode >> toString
 
let part1  = List.map toDec >> List.sum >> snafu

let part2 _ = None

let Solve (xs:string list) = xs |> parse |> both part1 part2
