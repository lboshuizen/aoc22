module Day21

module AS =
     // hard work done by theses guys :-)
     open AngouriMath.FSharp.Core
     let solveEq s =
          let exp = parsed s
          let s' = exp.Solve("humn")
          s'.ToString() // not optimal but provided an answer

type Expr =
     | Const of int64
     | Var of string
     | Node of Expr * string * Expr
     with override this.ToString() = match this with
                                     | Const c -> c.ToString()
                                     | Var v -> v
                                     | Node (a,o,b) -> $"({a} {o} {b})"
  
let opr = function
     | "+" -> fun a b -> (a+b)
     | "*" -> fun a b -> (a*b)
     | "-" -> fun a b -> (a-b)
     | "/" -> fun a b -> (a/b)

let parse =
     let monkey (s:string) = match s with
                             | _ when isDigit s[0] -> Const (int64 s)
                             | _ -> s |> splitOn ' ' |> fun a -> Node (Var a[0],a[1], Var a[2] )
     List.map (splitOn ':' >> fun a -> (a[0],a[1] |> trim |> monkey)) >> Map 

let rec reduce (m:Map<_,_>) ex = function
                                 | Const n -> Const n
                                 | Var a when a=ex -> Var a // isolate var in part 2
                                 | Var a -> reduce m ex m[a]
                                 | Node (Const a,o, Const b) -> (opr o) a b |> Const
                                 | Node (a,o,b) -> Node (reduce m ex a,o, reduce m ex b)

let simplify ex (m:Map<_,_>) = until (fun e -> reduce m ex e = e) (reduce m ex) m["root"]

let part1 = simplify "" >> fun (Const c) -> c 
    
let part2 =
     let rewriteRoot (Node (a,_,b)) = Node (a,"=",b)
     simplify "humn" >> rewriteRoot >> fun a -> a.ToString() |> AS.solveEq
                                        
let Solve = parse >> both part1 part2