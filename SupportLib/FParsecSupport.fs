namespace SupportLib

module FParsecSupport =
    open FParsec

    let str s = pstring s
    let ws = spaces
    let str_ws s = pstring s .>> ws

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn $"Success: %A{result}"
        | Failure(errorMsg, _, _) -> printfn $"Failure: %s{errorMsg}"

