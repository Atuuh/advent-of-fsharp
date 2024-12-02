module AOC.Year2024.Day2

open AOC
open AOC.List

let parseInput input =
    input |> Input.mapToList (fun x -> x |> String.split ' ' |> List.map int32) '\n'

let normaliseDifferences diffs =
    let isIncrementing = diffs |> List.sum > 0

    if isIncrementing then diffs else List.map ((*) -1) diffs

let getPassingDifferences report =
    let differences = consecutiveDifference report //zipped |> List.map (fun ([ a; b ]) -> b - a)
    let normalisedDifferences = normaliseDifferences differences
    normalisedDifferences |> List.map (fun x -> x > 0 && x <= 3)

let isSafe report =
    report |> getPassingDifferences |> List.forall ((=) true)

let isSafeDampened originalReport =
    let differences = getPassingDifferences originalReport
    let failures = differences |> List.filter ((=) false)

    match List.length failures with
    | 0 -> true
    | 1
    | 2 ->
        let failingIndex = differences |> List.findIndex ((=) false)

        let left = originalReport |> List.removeAt failingIndex
        let right = originalReport |> List.removeAt (failingIndex + 1)

        isSafe left || isSafe right
    | _ -> false

let private partA input =
    let reports = parseInput input
    let reportSafety = reports |> List.map isSafe

    reportSafety
    |> List.filter ((=) true)
    |> List.length
    |> printfn "Safe Reports: %i"

let private partB input =
    let reports = parseInput input
    let reportSafety = reports |> List.map isSafeDampened

    reportSafety
    |> List.filter ((=) true)
    |> List.length
    |> printfn "Safe Reports: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
