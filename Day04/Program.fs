open FParsec
open SupportLib.FileReading
open SupportLib.StringHelpers
open SupportLib.FParsecSupport

let input = readAllLines "example_input.txt"

let xmasCountParser: Parser<string, unit> = 
    str "XMAS" <|> str "SAMX"

let countXmases (input:string) : int =
    let rec loop (partial:string) (acc:int) =
        match partial with
        | _ when partial.Length = 0 -> acc
        | _ ->
            let newAcc = match run xmasCountParser partial with
                         | Success _ -> acc + 1
                         | _ -> acc
            loop (partial.Substring(1)) newAcc
    loop input 0

let getFirstDiaIndexes width rowIndex colIndex=
    let rec loop iRow iCol acc =
        match colIndex with
        | _ when iCol = width -> acc |> List.rev
        | _ -> loop (iRow + 1) (iCol + 1) ((iRow, iCol)::acc)
    loop rowIndex colIndex []
    
let getFirstRevDiaIndexes width rowIndex colIndex=
    let rec loop iRow iCol acc =
        match colIndex with
        | _ when iCol < 0 -> acc |> List.rev
        | _ -> loop (iRow + 1) (iCol - 1) ((iRow, iCol)::acc)
    loop rowIndex colIndex []

let projectDiagonally (input: string list)=
    let charArray = stringListTo2DCharArray input
    let width = Array2D.length2 charArray
    for i in 0 .. width - 1 do
        printfn $"%A{getFirstDiaIndexes width 0 i}"
    for j in 0 .. width - 1 do
        printfn $"%A{getFirstRevDiaIndexes width 0 (width - 1 - j)}"
    
projectDiagonally input

let countManyXmases (input:string list) : int = 
    input
    |> List.append (projectVertically input)
    |> List.map countXmases
    |> List.sum
    
printfn $"%d{countManyXmases input}"
