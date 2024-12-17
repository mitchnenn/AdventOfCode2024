namespace SupportLib

module StringHelpers =
    let charListToString (chars: char list) : string =
        new string(Array.ofList chars)

    let stringToCharList (input: string) : char list =
        input |> Seq.toList
    
    let revString (input: string) : string =
        new string(input.ToCharArray() |> Array.rev)
    
    let countSubstring (substring: string) (input: string) : int =
        let rec loop (partial: string) (acc: int) =
            match partial.IndexOf(substring) with
            | -1 -> acc
            | index -> loop (partial.Substring(index + substring.Length)) (acc + 1)
        loop input 0
    
    let stringListTo2DCharArray (input: string list) : char[,] =
        let width = input |> List.map (_.Length) |> List.max
        let height = List.length input
        let result = Array2D.init height width (fun _ _ -> ' ')
        for rowIndex in 0 .. height - 1 do
            let row = input.[rowIndex]
            for colIndex in 0 .. row.Length - 1 do
                result.[rowIndex, colIndex] <- input.[rowIndex].[colIndex]
        result

    let appendReverse (input: string list) : string list =
        input |> List.append (input |> List.map revString)
        
    let getVerticals (input: string list) : string list =
        let charArray = stringListTo2DCharArray input
        let height = Array2D.length1 charArray
        let width = Array2D.length2 charArray
        [for col in 0 .. width - 1 ->
            let chars = [for row in 0 .. height - 1 -> charArray.[row, col]]
            new string(Array.ofList chars)]
        
    
    // For a square matrix of size ( n \times n ):  
    // The number of top-left to bottom-right diagonals is ( 2n - 1 ).
    // The number of bottom-left to top-right diagonals is also ( 2n - 1 ).
    let getDiagonals (input: string list) : string list =
        let charArray = stringListTo2DCharArray input
        let height = Array2D.length1 charArray
        let width = Array2D.length2 charArray
        let getDiagonal (startRow, startCol) (rowStep, colStep) =
            let rec loop (row, col) acc =
                if row >= 0 && row < height && col >= 0 && col < width then
                    loop (row + rowStep, col + colStep) (charArray.[row, col] :: acc)
                else
                    acc |> List.rev |> Array.ofList |> System.String
            loop (startRow, startCol) []
        
        let topLeftToBottomRight =
            [for row in 0 .. height - 1 -> getDiagonal (row, 0) (1, 1)]
            @ [for col in 1 .. width - 1 -> getDiagonal (0, col) (1, 1)]
        
        let bottomLeftToTopRight =
            [for row in 0 .. height - 1 -> getDiagonal (row, 0) (-1, 1)]
            @ [for col in 1 .. width - 1 -> getDiagonal (height - 1, col) (-1, 1)]
        
        topLeftToBottomRight @ bottomLeftToTopRight
        
    
    