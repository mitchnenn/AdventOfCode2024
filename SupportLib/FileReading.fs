namespace SupportLib

module FileReading =
    open System.IO
    
    let readAllText path = File.ReadAllText(path)
    