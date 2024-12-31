open SupportLib.FileReading
open SupportLib.CharAndIndex

let input = readAllLines "example_input.txt"
//let input = readAllLines "problem_input.txt"

let xmas:string = "MAS" 

let charAndIndexArray = stringListTo2DCharIndexArray (xmas.ToCharArray() |> List.ofArray) input

charAndIndexArray
|> getCharIndexDiagonals
|> List.map (allSubstringCharIndexes xmas)
|> List.collect id
|> List.filter (fun x -> x.Char = 'A')
|> List.countBy (fun x -> x.Row,x.Col)
|> List.filter (fun (_,count) -> count > 1)
|> List.length
|> printfn "%d"

