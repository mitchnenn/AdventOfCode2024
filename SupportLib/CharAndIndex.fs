namespace SupportLib

module CharAndIndex =
    open ArrayHelpers
    open StringHelpers
    
    ///////////////////////// CharAndIndex Helpers /////////////////////////////
    
    let defaultChar = '.'

    type CharAndIndex = { Char:char; Row:int; Col:int } 

    let charAndIndexToString (charAndIndexes: CharAndIndex list) : string =
        charAndIndexes |> List.map (_.Char) |> charListToString

    let indexOfAllCharIndexes (subString:string) (charIndexes: CharAndIndex list) : CharAndIndex list =
        let stringToSearch = charAndIndexToString charIndexes
        let matchIndexes = stringToSearch |> indexOfAll subString
        matchIndexes |> List.map (fun x -> charIndexes.[x])
    
    let charAndIndexContains (subString:string) (charIndexes: CharAndIndex list) : bool =
        charIndexes
        |> indexOfAllCharIndexes subString
        |> List.length > 0

    let allSubstringCharIndexes (subString:string) (charIndexes: CharAndIndex list) : CharAndIndex list =
        let matchIndexes = charIndexes
                           |> charAndIndexToString
                           |> indexOfAll subString
        let rec loop (indexes: int list) (acc: CharAndIndex list) =
            match indexes with
            | [] -> acc
            | x::xs ->
                let charIndexSubstring = charIndexes.[x..(x + subString.Length - 1)]
                loop xs (acc @ charIndexSubstring)
        loop matchIndexes []
                            
    ///////////////////////// 2D Array CharAndIndex Helpers /////////////////////////////
    
    let stringListTo2DCharIndexArray (matchArray:char list) (input: string list) : CharAndIndex[,] =
        let width = input |> List.map (_.Length) |> List.max
        let height = List.length input
        let inputArray = stringListTo2DCharArray input
        Array2D.init height width (fun r c ->
            {
                Char = (if (matchArray |> List.contains inputArray.[r,c]) then inputArray.[r,c] else defaultChar)
                Row = r
                Col = c
            })
        
    let charIndexArrayToStringList (charIndexArray:CharAndIndex[,]) : string list =
        let height = Array2D.length1 charIndexArray
        let width = Array2D.length2 charIndexArray
        [for row in 0 .. height - 1 -> 
            [for col in 0 .. width - 1 ->
                charIndexArray.[row, col].Char] |> charListToString] 
    
    ///////////////////////// Diagonal CharAndIndex Helpers /////////////////////////////
        
    let getCharIndexDiagonals (input: CharAndIndex[,]) =
        let topLeft = getTopLeftDiagonals input
        let bottomLeft = getBottomLeftDiagonals input
        let topRight = getTopRightDiagonals input
        let bottomRight = getBottomRightDiagonals input
        topLeft @ bottomLeft @ topRight @ bottomRight