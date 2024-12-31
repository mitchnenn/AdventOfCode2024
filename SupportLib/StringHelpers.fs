namespace SupportLib

module StringHelpers =
    open ArrayHelpers
    
    let charListToString (chars: char list) : string =
        chars
        |> Array.ofList
        |> System.String
        
    let stringToCharList (input: string) : char list =
        input |> Seq.toList
    
    let revString (input: string) : string =
        input
        |> stringToCharList
        |> List.rev
        |> charListToString

    let appendReverseString (input: string list) : string list =
        input
        |> List.append (input |> List.map revString)
    
    let indexOfAll (subString:string) (input:string) : int list =
        let rec loop (startIndex: int) (acc: int list) =
            match input.IndexOf(subString, startIndex) with
            | -1 -> acc |> List.rev
            | index -> loop (index + subString.Length) (index :: acc)
        loop 0 []
    
    let countSubstring (substring: string) (input: string) : int =
        indexOfAll substring input |> List.length
    
    ///////////////////////// 2D Char Array Helpers /////////////////////////////        
    
    let stringListTo2DCharArray (input: string list) : char[,] =
        let width = input |> List.map (_.Length) |> List.max
        let height = List.length input
        let result = Array2D.init height width (fun _ _ -> ' ')
        for rowIndex in 0 .. height - 1 do
            let row = input.[rowIndex]
            for colIndex in 0 .. row.Length - 1 do
                result.[rowIndex, colIndex] <- input.[rowIndex].[colIndex]
        result

    ///////////////////////// Diagonal Char Helpers /////////////////////////////
    
    let getDiagonals (input: string list) =
        let charArray = stringListTo2DCharArray input
        let topLeft = getTopLeftDiagonals charArray
        let bottomLeft = getBottomLeftDiagonals charArray
        let topRight = getTopRightDiagonals charArray
        let bottomRight = getBottomRightDiagonals charArray
        topLeft @ bottomLeft @ topRight @ bottomRight
        
        