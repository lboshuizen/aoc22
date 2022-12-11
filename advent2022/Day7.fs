// Works... but screams for refactoring
module Day7

let fileOrFolder p =
    let fileFromString = splitOn ' ' >> fun a -> (int a[0])
    // only interested in file-sizes, folders can be ignored
    let folderFromString = splitOn ' ' >> fun a -> a[1]
                     
    function
    | (s:string) when isDigit s[0] -> (p,(fileFromString s))
    | s -> ((folderFromString s)::p,0)

let ls p xs =
    let notCmd (s:string) = s.StartsWith "$" |> not
    let fs = xs |> List.takeWhile notCmd |> List.map (fileOrFolder p)
    xs |> (List.skipWhile notCmd), fs

// should(!) be a foldl over
// string list -> (string list) * (string list * int) list -> (string list * int) list
let rec interpret (path:string list) (acc:(string list * int) list) (xs:string list) =
        
    let cd p a xs = interpret p a xs
                                           
    match xs with
    | [] -> acc
    | cmd::r when cmd.StartsWith "$ cd" -> cmd |> splitOn ' ' |> fun a -> match a[2] with
                                                                          | ".." -> cd (List.tail path) acc r
                                                                          | n -> cd (n::path) acc r

    // ls can be ignored, filter subsection on starting digits                                                                          
    | cmd::r when cmd.StartsWith "$ ls" -> let (t,fs) = ls path r 
                                           interpret path (acc@fs) t
                                                                          
    | _::r -> interpret path acc r

let parse = interpret [""] []

let propagate (fs:Map<string list,int>) =
    let parent = List.rev >> List.tail >> List.rev
    let addSize s = function
                    | Some v' -> Some (v'+s)
                    | _ -> None
                                    
    let updateParent m p = match Map.tryFind (parent p) m with
                           | None ->   Map.add (parent p) m[p] m
                           | Some _ -> Map.change (parent p) (addSize m[p]) m
                                    
    Map.keys fs |> Seq.sortByDescending List.length |> Seq.fold updateParent fs 

let buildFs =
    let normalizePath = List.rev >> List.tail
    let folderSize = List.map snd >> List.sum
    
    List.groupBy fst >> List.map (fun (p,xs) -> (normalizePath p, folderSize xs))
    >> Map >> propagate
    >> fun fs -> (fs.Values |> List.ofSeq, fs[["/"]] + 30000000 - 70000000) 
        
let part1 = buildFs >> fst >> Seq.filter (fun s -> s <= 100000) >> Seq.sum

let toFree (fs,f) = fs |> List.filter (fun s -> s >= f)
    
let part2 = buildFs >> toFree >> List.min 

let Solve (xs:string list) = xs |> parse |> both part1 part2