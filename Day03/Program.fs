open FParsec
open Microsoft.FSharp.Core
open SupportLib.FileReading
open SupportLib.FParsecSupport

//let input = readAllText "example_input.txt"
let input = readAllText "problem_input.txt"

let firstParser: Parser<int, unit> =
    str "mul(" >>. pint32
    
let secondParser: Parser<int, unit> =
    str "," >>. pint32 .>> str ")"

let mulOperandParser: Parser<int, unit> =
    firstParser .>>. secondParser
    |>> fun (first,second) -> first * second
    
let parseMultiplications (input: string) : int =
    let rec loop (input:string) (enabled:bool) (acc:int) : int =
        match input with
        | _ when input.Length = 0 -> acc
        | _ when input.StartsWith("don't()") -> loop (input.Substring(7)) false acc
        | _ when input.StartsWith("do()") -> loop (input.Substring(4)) true acc
        | _ -> let updatedAcc = match run mulOperandParser input with
                                | Success (result,_,_) when enabled -> result + acc
                                | _ -> acc
               loop (input.Substring(1)) enabled updatedAcc
    loop input true 0
        
printfn $"%d{parseMultiplications input}"