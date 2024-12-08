namespace SupportLib

module IntListHandling =
    let parseSequentialDifferences (nums: int list) : int list =
        nums
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)
