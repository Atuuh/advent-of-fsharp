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

let getAllEquations getOptionsFn numbers target =
    let rec loop remainingNumbers results =
        match remainingNumbers, results with
        | next :: tail, [] -> loop tail ([ next ])
        | [], _ -> List.exists ((=) target) results
        | next :: tail, _ ->
            if next > target then
                false
            else
                loop tail (results |> List.collect (getOptionsFn next))

    loop numbers []

let private partA input =
    let equations = parseInput input
    let getOptions a b = [ a * b; a + b ]

    let answer =
        equations
        |> List.filter (fun (target, numbers) -> getAllEquations getOptions numbers target)
        |> List.sumBy fst
        |> uint64

    printfn "Answer: %i" answer

let private partB input =
    let equations = parseInput input

    let getOptions a b =
        [ a * b; a + b; b.ToString() + a.ToString() |> double ]

    let answer =
        equations
        |> List.filter (fun (target, numbers) -> getAllEquations getOptions numbers target)
        |> List.sumBy fst
        |> uint64

    printfn "Answer: %i" answer

    printfn "Answer: %i" 0

let solution: Types.Solution = { partA = partA; partB = partB }
