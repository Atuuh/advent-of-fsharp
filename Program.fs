module AOC.Main

open System.Diagnostics

let getArgs (args: string[]) =
    try
        ((args[0] |> int), (args[1] |> int))
    with _ ->
        printfn "Failed getting arguments"
        exit 1


let timeOperation func args =
    let timer = new Stopwatch()
    timer.Start()
    let returnedValue = func args
    timer.Stop()
    printfn "Timer took %ims" timer.ElapsedMilliseconds
    returnedValue

[<EntryPoint>]
let main args =
    // let (year, day) = (2016, 7)
    let (year, day) = getArgs args
    let t = System.Type.GetType $"AOC.Year{year}.Day{day}"
    let solution = t.GetProperty("solution").GetValue(null) :?> Types.Solution

    let input = Input.getInput year day

    printfn $"{year} Day {day} Part A"
    timeOperation solution.partA input

    printfn $"{year} Day {day} Part B"
    timeOperation solution.partB input

    0
