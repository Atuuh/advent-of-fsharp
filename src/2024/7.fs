module AOC.Year2024.Day7

open AOC

let parseInput input =
    input
    |> Input.mapToList
        (fun line ->
            let items = line |> String.splits ": "

            match items with
            | [ a; b ] -> double a, b |> String.splits " " |> List.map double
            | _ -> failwith "Parsing input failed")
        '\n'

let flip f a b = f b a

let rec power n x =
    if n = 0 then x else power (n - 1) (x * x)

let getAllOptions a b = [ a * b; a + b ]

let getAllOptions2 a b =
    [ a * b; a + b; b.ToString() + a.ToString() |> double ]

let getAllEquations getOptionsFn numbers =
    let rec loop remainingNumbers results =
        match remainingNumbers, results with
        | next :: tail, [] -> loop tail ([ next ])
        | [], _ -> results
        | next :: tail, _ -> loop tail (results |> List.collect (getOptionsFn next))

    loop numbers []

let private partA input =
    let equations = parseInput input

    let answer =
        equations
        |> List.choose (fun (result, numbers) ->
            let allResults = getAllEquations getAllOptions numbers
            let doAnyResultsMatch = allResults |> List.exists ((=) result)

            if doAnyResultsMatch then Some result else None)
        |> List.sum
        |> uint64

    printfn "Answer: %i" answer

let private partB input =
    let equations = parseInput input

    let answer =
        equations
        |> List.choose (fun (result, numbers) ->
            let allResults = getAllEquations getAllOptions2 numbers
            let doAnyResultsMatch = allResults |> List.exists ((=) result)

            if doAnyResultsMatch then Some result else None)
        |> List.sum
        |> uint64

    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
