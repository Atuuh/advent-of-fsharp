module AOC.Year2024.Day5

open AOC
open AOC.String

let parseRule value =
    match value with
    | ParseRegex "(\d+)\|(\d+)" [ Integer a; Integer b ] -> a, b
    | _ -> failwith "Error parsing rule"

let parsePages value =
    value |> String.splits "," |> List.map int

let parseInput input =
    input
    |> String.splits "\n\n"
    |> (fun items ->
        match items with
        | [ rules; pages ] ->
            let r = rules |> String.splits "\n" |> List.map parseRule |> Set.ofList
            let p = pages |> String.splits "\n" |> List.map parsePages
            r, p
        | _ -> failwith "Error parsing input data")

let getIndex xs x = List.tryFindIndex ((=) x) xs

let compareFn rules a b =
    if Set.contains (a, b) rules then -1
    elif Set.contains (b, a) rules then 1
    else 0

let isSortedCorrectly rules (page: int list) =
    page |> List.pairwise |> List.forall (fun (a, b) -> compareFn rules a b < 0)

let getMiddle xs = List.item (List.length xs / 2) xs

let private partA input =
    let rules, pages = parseInput input
    let correctlySortedPages = pages |> List.filter (isSortedCorrectly rules)
    let answer = correctlySortedPages |> List.map getMiddle |> List.sum
    printfn "Answer: %i" answer

let private partB input =
    let rules, pages = parseInput input
    let incorrectlySortedPages = pages |> List.filter ((isSortedCorrectly rules) >> not)

    let sortedPages =
        incorrectlySortedPages |> List.map (List.sortWith (compareFn rules))

    let answer = sortedPages |> List.map getMiddle |> List.sum
    printfn "Answer: %i" answer


let solution: Types.Solution = { partA = partA; partB = partB }
