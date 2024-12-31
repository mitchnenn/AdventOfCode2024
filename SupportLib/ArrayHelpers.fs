namespace SupportLib

module ArrayHelpers =
 
    let getDiagonal (startRow, startCol) (rowStep, colStep) (input:'a[,]) : 'a list =
        let height = Array2D.length1 input
        let width = Array2D.length2 input
        let rec loop (row, col) acc =
            if row >= 0 && row < height && col >= 0 && col < width then
                loop (row + rowStep, col + colStep) (input.[row, col] :: acc)
            else
                acc |> List.rev
        loop (startRow, startCol) []
        
    let getTopLeftDiagonals (input: 'a[,]) =
        let height = Array2D.length1 input
        let width = Array2D.length2 input
        [for row in 0 .. height - 1 -> getDiagonal (row, 0) (1, 1) input]
        @ [for col in 1 .. width - 1 -> getDiagonal (0, col) (1, 1) input]
    
    let getBottomLeftDiagonals (input: 'a[,]) =
        let height = Array2D.length1 input
        let width = Array2D.length2 input
        [for row in 0 .. height - 1 -> getDiagonal (height - 1 - row, 0) (-1, 1) input]
        @ [for col in 1 .. width - 1 -> getDiagonal (height - 1, col) (-1, 1) input]
        
    let getTopRightDiagonals (input: 'a[,]) =
        let height = Array2D.length1 input
        let width = Array2D.length2 input
        [for row in 0 .. height - 1 -> getDiagonal (row, width - 1) (1, -1) input]
        @ [for col in 1 .. width - 1 -> getDiagonal (0, width - 1 - col) (1, -1) input]
        
    let getBottomRightDiagonals (input: 'a[,]) =
        let height = Array2D.length1 input
        let width = Array2D.length2 input
        [for row in 0 .. height - 1 -> getDiagonal (height - 1 - row, width - 1) (-1, -1) input]
        @ [for col in 1 .. width - 1 -> getDiagonal (height - 1, width - 1 - col) (-1, -1) input]