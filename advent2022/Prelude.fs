[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System.Text.RegularExpressions

let flip f a b = f b a

let foldl = Seq.fold
let foldr f = flip (List.foldBack f) // Who(??) decided to give foldBack that crazy signature

let splitOn (c:char) (s:string) = s.Split c

let reorder (xs:'a list list) = xs |> (List.map List.rev >> List.rev)

let splitWhen (pred:'a->bool) =
    let splitter c (r,f) = match pred c with
                           | true -> ([],r::f)
                           | _ -> (c::r,f)
                           
    foldr splitter ([],[]) >> fun (r,f) -> r::f

/// <summary>Parse a string with pattern <c>regex</c> and transforms into result with <c>map</c></summary>
/// <param name="pat">Pattern to match</param>
/// <param name="map">Mapper (f:string[] -> 'a) to result</param>
/// <returns>result of <c>map</c></returns>
/// <remarks>Does NOT adapt to failing matches, is expected to throw!</remarks>
let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map

let splitOnEmpty = splitWhen ((=)"")

let both f g x = (f x, g x)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let tOp = uncurry 