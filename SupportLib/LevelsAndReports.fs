namespace SupportLib

module LevelsAndReports =
    open FParsec
    open IntListHandling
    
    type Report = {
        Levels: int32 list
    }

    let levelParser: Parser<int32 list, unit> =
        many (pint32 .>> opt (pchar ' ')) .>> newline
        
    let reportParser: Parser<Report, unit> =
        levelParser
        |>> fun levels -> {Levels = levels}
      
    let reportListParser: Parser<Report list, unit> =
        many reportParser

    let parseReports (input:string): Result<Report list, string> =
        match run reportListParser input with
        | Success(result, _, _) -> Result.Ok(result)
        | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

    let isSafeReport (diffs:int list) : bool =
        let maxStep = diffs |> List.map abs |> List.max
        let allPositive = List.forall(fun i -> i > 0) diffs
        let allNegative = List.forall(fun i -> i < 0) diffs
        (maxStep <= 3) && (allPositive || allNegative)
        
    let findSafeReports (reports : Report list) =
        reports
        |> List.map (fun r -> (r, parseSequentialDifferences r.Levels))
        |> List.map (fun (r,diffs) -> r, isSafeReport diffs)
        |> List.filter (fun (_, s) -> s = true)
        |> List.map fst