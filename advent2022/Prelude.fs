[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

let splitOn (c:char) (s:string) = s.Split c

let reorder (xs:'a list list) = xs |> (List.map List.rev >> List.rev)

let splitWhen (pred:'a->bool) =
    let splitter (r,f) c = match pred c with
                           | true -> ([],r::f)
                           | _ -> (c::r,f)
                           
    List.fold splitter ([],[]) >> fun (r,f) -> r::f

let flip f a b = f b a

let splitOnEmpty = splitWhen ((=)"") >> reorder

let both f g x = (f x, g x)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let tOp = uncurry 