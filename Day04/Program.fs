open FParsec
open SupportLib.FileReading
open SupportLib.StringHelpers
open SupportLib.FParsecSupport

//let input = readAllLines "example_input.txt"
let input = readAllLines "problem_input.txt"

let xmas:string = "XMAS" 

let verticals = input |> getVerticals |> appendReverse
let diagonals = input |> getDiagonals |> appendReverse

let count =
    input
    |> appendReverse
    |> List.append verticals
    |> List.append diagonals
    |> List.map (countSubstring xmas)
    |> List.sum
    
printfn $"Count: %d{count}"
