namespace SupportLib

module IntListHandling =
    let parseSequentialDifferences (nums: int list) : int list =
        nums
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)

    let exceptItem (index:int) (aList:int list) : int list =
        aList
        |> List.mapi(fun i x -> (i,x))
        |> List.filter(fun (i,_) -> i <> index)
        |> List.map snd
        
    let buildExceptedIndexList (input:int list) : int list list =
        let exceptedLists =
            input
            |> List.mapi(fun index _ -> exceptItem index input)
        input :: exceptedLists
    