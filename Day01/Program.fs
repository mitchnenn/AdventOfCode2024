open SupportLib.FileReading
open SupportLib.ParseListPairs


//let input = readAllText "example_input.txt"
let input = readAllText "problem_input.txt"

let listPairs =
    match parseListPairs input with
        | Result.Ok(records) -> records 
        | Result.Error(errorMsg) -> printfn $"List parse error: %s{errorMsg}"; []
        
let left = getLeft listPairs |> List.sort 
let right = getRight listPairs |> List.sort
let calcDistance = List.map2 (-)
let distances = calcDistance left right |> List.map(abs) |> List.sum 
printfn $"%A{distances}"

let HowManySatisfy pred = Seq.filter pred >> Seq.length 
let similarityScore = List.map2(fun first _ ->
    first, right |> HowManySatisfy(fun elem -> elem = first)) left right
                    |> List.map (fun t -> fst(t) * snd(t))
                    |> List.sum
printfn $"%A{similarityScore}"