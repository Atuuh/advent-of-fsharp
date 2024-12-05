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
            let r = rules |> String.splits "\n" |> List.map parseRule
            let p = pages |> String.splits "\n" |> List.map parsePages
            r, p
        | _ -> failwith "Error parsing input data")

let getIndex xs x = List.tryFindIndex ((=) x) xs

let isSortedCorrectly rules page =
    rules
    |> List.forall (fun (a, b) ->
        match getIndex page a, getIndex page b with
        | Some x, Some y -> x < y
        | _ -> true)

let getMiddle xs = List.item (List.length xs / 2) xs

let print msg x =
    printfn msg x
    x

let sortRules rules =
    let numbersInRules = rules |> List.collect (fun (a, b) -> [ a; b ]) |> List.distinct

    let sorter (a, b) x y =
        match a, b, x, y with
        | a, b, x, y when a = x && b = y -> -1
        | a, b, x, y when a = y && b = x -> -1
        | _ -> 0


    List.fold
        (fun nums rule -> List.sortWith (sorter rule) nums |> (print "sort step: rule=%A %A" rule))
        numbersInRules
        rules

let sortPage rules (a: int) (b: int) =
    match getIndex rules a, getIndex rules b with
    | Some x, Some y -> compare x y
    | _ -> 0

let private partA input =
    let rules, pages = parseInput input
    let correctlySortedPages = pages |> List.filter (isSortedCorrectly rules)
    let answer = correctlySortedPages |> List.map getMiddle |> List.sum
    printfn "Answer: %i" answer

let private partB input =
    let rules, pages = parseInput input
    let incorrectlySortedPages = pages |> List.filter (isSortedCorrectly rules >> not)
    printfn "Incorrect pages %A" incorrectlySortedPages
    let sortedRules = sortRules rules
    printfn "Sorted rules %A" sortedRules

    let sortedPages =
        incorrectlySortedPages |> List.map (List.sortWith (sortPage sortedRules))

    printfn "Correct pages %A" sortedPages
    let answer = sortedPages |> List.map getMiddle |> List.sum
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }

// 97,75,47,61,53,29,13
