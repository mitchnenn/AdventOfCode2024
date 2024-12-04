namespace SupportLib

module FParsecTest =
    open FParsec
    
    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn $"Success: %A{result}"
        | Failure(errorMsg, _, _) -> printfn $"Failure: %s{errorMsg}"
