namespace SupportLib

module ParseListPairs =
    open FParsec

    type ListPair = {
        Left: int
        Right: int
    }    
        
    let leftParser: Parser<int, unit> =
        pint32 .>> spaces
        
    let rightParser: Parser<int, unit> =
        pint32 .>> restOfLine true
        
    let lineParser: Parser<ListPair, unit> =
        leftParser .>>. rightParser
        |>> fun (left,right) -> {Left = left; Right = right}
        
    let manyLineParser: Parser<ListPair list, unit> =
        many lineParser

    let parseListPairs (input: string): Result<ListPair list, string> =
        let trimmedInput = input.Trim()
        match run manyLineParser trimmedInput with
        | Success(result, _, _) -> Result.Ok(result)
        | Failure(errorMsg, _, _) -> Result.Error(errorMsg)
    
    let getLeft (pairs: ListPair list) : int list =
        pairs |> List.map (_.Left)
        
    let getRight(pairs: ListPair list) : int list =
        pairs |> List.map (_.Right)
        