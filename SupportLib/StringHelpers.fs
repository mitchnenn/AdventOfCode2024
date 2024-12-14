namespace SupportLib

module StringHelpers =
    let charListToString (chars: char list) : string =
        new string(Array.ofList chars)

    let stringToCharList (input: string) : char list =
        input |> Seq.toList
    
    let projectVertically (strings: string list) =
        let maxLength = strings |> List.map String.length |> List.max
        let mutable acc = []
        for i in 0 .. maxLength - 1 do
            let line = strings
                       |> List.map (fun s -> if i < s.Length then s.[i] else ' ')
                       |> charListToString
            acc <- line :: acc
        acc |> List.rev
        
    let stringListTo2DCharArray (input: string list) : char[,] =
        let width = input |> List.map (_.Length) |> List.max
        let height = List.length input
        let result = Array2D.init height width (fun _ _ -> ' ')
        for rowIndex in 0 .. height - 1 do
            let row = input.[rowIndex]
            for colIndex in 0 .. row.Length - 1 do
                result.[rowIndex, colIndex] <- input.[rowIndex].[colIndex]
        result
