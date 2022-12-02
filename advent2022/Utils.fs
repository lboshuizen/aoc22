[<Microsoft.FSharp.Core.AutoOpen>]
module Utils

let splitOn (c:char) (s:string) = s.Split c

let splitWhen (pred:'a->bool) (xs: 'a list) =
    let rec split xs s a =
        match xs with
        | x::r when pred x -> split r [] (s::a)
        | x::r -> split r (x::s) a
        | _ -> (s::a)
    
    split xs [] [] |> List.rev |> List.map (List.rev)

let both f g x = (f x, g x)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let tOp = uncurry 