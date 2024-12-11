open FParsec
open Microsoft.FSharp.Core
open SupportLib.FileReading
open SupportLib.FParsecSupport

//let input = readAllText "example_input.txt"
let input = readAllText "problem_input.txt"

let mulInst = "mul("
let dontInst = "don't()"
let doInst = "do()"

let firstParser: Parser<int, unit> =
    str mulInst >>. pint32
    
let secondParser: Parser<int, unit> =
    str "," >>. pint32 .>> str ")"

let mulOperandParser: Parser<int, unit> =
    firstParser .>>. secondParser
    |>> fun (first,second) -> first * second
    
let parseMultiplications (input: string) : int =
    let rec loop (input:string) (enabled:bool) (acc:int) : int =
        match input with
        | _ when input.Length = 0 -> acc
        | _ when input.StartsWith(dontInst) -> loop (input.Substring(dontInst.Length)) false acc
        | _ when input.StartsWith(doInst) -> loop (input.Substring(doInst.Length)) true acc
        | _ -> let updatedAcc = match run mulOperandParser input with
                                | Success (result,_,_) when enabled -> result + acc
                                | _ -> acc
               loop (input.Substring(1)) enabled updatedAcc
    loop input true 0
        
printfn $"%d{parseMultiplications input}"