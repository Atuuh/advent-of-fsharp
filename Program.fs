module AOC.Main

let solutionsMap =
    Map(
        [ (2016,
           Map[(1, Year2016.Day1.solution)
               (2, Year2016.Day2.solution)
               (3, Year2016.Day3.solution)
               (4, Year2016.Day4.solution)
               (5, Year2016.Day5.solution)
               (6, Year2016.Day6.solution)
               (7, Year2016.Day7.solution)

               ]) ]
    )

let getArgs (args: string[]) =
    try
        ((args[0] |> int), (args[1] |> int))
    with _ ->
        printfn "Failed getting arguments"
        exit 1


let getSolution (year: int) (day: int) =
    try
        solutionsMap[year][day]
    with _ ->
        printfn $"{year} day {day} is not set up!"
        exit 1

[<EntryPoint>]
let main args =
    let (year, day) = (2016, 7)
    // let (year, day) = getArgs args
    let solution = getSolution year day
    let input = Input.getInput year day

    printfn $"{year} Day {day} Part A"
    solution.partA input

    printfn $"{year} Day {day} Part B"
    solution.partB input

    0
