module AOC.Main

let getArgs (args: string[]) =
    try
        ((args[0] |> int), (args[1] |> int))
    with _ ->
        printfn "Failed getting arguments"
        exit 1

[<EntryPoint>]
let main args =
    // let (year, day) = (2016, 7)
    let (year, day) = getArgs args
    let t = System.Type.GetType $"AOC.Year{year}.Day{day}"
    let solution = t.GetProperty("solution").GetValue(null) :?> Types.Solution

    let input = Input.getInput year day

    printfn $"{year} Day {day} Part A"
    solution.partA input

    printfn $"{year} Day {day} Part B"
    solution.partB input

    0
