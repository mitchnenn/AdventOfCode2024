open SupportLib.FileReading
open SupportLib.ParseListPairs


let exampleInput = readAllText "example_input.txt"
let listPairs =
    match parseListPairs exampleInput with
        | Result.Ok(records) -> records 
        | Result.Error(errorMsg) -> printfn $"List parse error: %s{errorMsg}"; []
        
let left = getLeft listPairs |> List.sort 
let right = getRight listPairs |> List.sort

printfn $"%A{left}"
printfn $"%A{right}"

let calcDistance = List.map2 (-)
let distances = calcDistance left right |> List.map(abs) |> List.sum 
printfn $"%A{distances}"
