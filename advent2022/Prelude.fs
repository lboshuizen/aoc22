[<Microsoft.FSharp.Core.AutoOpen>]
module Prelude

open System.Collections.Generic
open System.Text.RegularExpressions

type Solver<'a> = string list -> 'a * 'a 

let toGrid2d (xs:#seq<#seq<'a>>) : ((int * int) * 'a) list = 
    let ri y = Seq.mapi (fun x a -> ((x,y),a)) >> List.ofSeq
    xs |> Seq.mapi ri |> Seq.concat |> List.ofSeq

let line2D ((x,y),(x2,y2)) = [for n in 0..max (abs (x2-x)) (abs (y2-y)) -> ((x+n*sign(x2-x)),y+n*sign(y2-y))]

let asInt (c:char) = int (c)-int ('0')

let flip f a b = f b a

let foldl = Seq.fold
let foldr f = flip (List.foldBack f) // Who(??) decided to give foldBack that crazy signature

let splitOnS (d:string) (s:string) = s.Split d
let splitOn (c:char) (s:string) = s.Split c

let reorder (xs:'a list list) = xs |> (List.map List.rev >> List.rev)

let splitWhen (pred:'a->bool) =
    let splitter c (r,f) = match pred c with
                           | true -> ([],r::f)
                           | _ -> (c::r,f)
                           
    foldr splitter ([],[]) >> fun (r,f) -> r::f

/// <summary>Parse a string with pattern <c>regex</c> and transforms into result with <c>map</c></summary>
/// <param name="pat">Pattern to match</param>
/// <param name="map">Mapper <c>(f:string[]->'a)</c> to result</param>
/// <returns>result of <c>map</c></returns>
/// <remarks>Does NOT adapt to failing matches, is expected to throw!</remarks>
let parseRegex regex map s =  Regex.Match(s,regex) |> fun m -> m.Groups
                              |> Seq.skip 1 // ignore first group
                              |> Seq.map (fun a -> a.Value) |> Array.ofSeq |> map

let splitOnEmpty = splitWhen ((=)"")

let toString (xs:seq<char>) = System.String.Concat xs

let both f g x = (f x, g x)

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let tOp = uncurry

let times n f i = Seq.fold (fun s _ -> f s) i [1..n]
let rec until p f i = match (p i) with
                      | true -> i
                      | false -> until p f (f i)
    
let mapSnd f (a,b) = (a, f b)
let mapFst f (a,b) = (f a, b)

let minus n a = a-n

let isDigit c = System.Char.IsDigit c

let inc = (+) 1
let dec = (flip (-)) 1
let dist (x,y) (x',y') = (x-x')*(x-x')+(y-y')*(y-y')

let rec gcd (a:int64) (b:int64) = if b = 0 then abs a else gcd (abs b) ((abs a) % abs b)

let lcm a b = (a*b) / (gcd a b)

let memo f =
    let cache = Dictionary<_,_>()
    fun c ->
        match cache.TryGetValue c with
        | true, v -> v
        | _ -> let value = f c
               cache.Add(c, value)
               value

let private ass s c v = match (v=c) with
                        | true -> ()
                        | false -> printfn $"%s{s} is incorrect: expected %A{c} got %A{v}"
                                   assert false

let shouldBe (c1:'a) (c2:'b) (a:'a,b:'b) = ass "part1" c1 a
                                           ass "part2" c2 b
                                           (a,b)