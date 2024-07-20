module AOC.Input

open System.IO

let trim (value: string) = value.Trim()

let getFilePath (year: int) (day: int) =
    Path.GetFullPath $"{__SOURCE_DIRECTORY__}/../../Inputs/{year}/{day}.txt"

let getInput year day =
    try
        let f = getFilePath year day
        getFilePath year day |> File.ReadAllText |> trim
    with ex ->
        printfn $"Error getting input file for year {year} day {day}"
        reraise ()

let toList (separator: char) (input: string) =
    input.Split separator |> Seq.map (fun x -> x.Trim()) |> Seq.toList

let mapToList mapFn separator input =
    toList separator input |> List.map mapFn
