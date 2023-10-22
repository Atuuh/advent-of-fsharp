module Input

open System.IO

let trim (value: string) = value.Trim()

let getFilePath (year: int) (day: int) =
    Path.GetFullPath $"Inputs/{year}/{day}.txt"

let getInput year day =
    try
        getFilePath year day |> File.ReadAllText |> trim
    with ex ->
        printfn $"Error getting input file for year {year} day {day}"
        reraise ()
