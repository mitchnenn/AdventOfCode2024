namespace SupportLib

module FileReading =
    open System.IO
    
    let readAllText path = File.ReadAllText(path)
    
    let readAllLines (path:string) : string list = File.ReadAllLines(path)
                                                |> Array.toList
    