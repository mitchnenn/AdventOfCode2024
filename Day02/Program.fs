open SupportLib.FileReading
open SupportLib.LevelsAndReports

//let input = readAllText "example_input.txt"
let input = readAllText "problem_input.txt"

let reports =
    match parseReports input with
    | Result.Ok(reports) -> reports
    | Result.Error(errorMsg) -> printfn $"Report parse error: %s{errorMsg}"; []

let safeReports = findSafeReports reports
printfn $"%A{safeReports}"

printfn $"%A{safeReports |> List.length}"