module AOC.Year2024.Day1

open AOC
open AOC.List
open AOC.String

let getNumbers line =
    match line with
    | ParseRegex "(\d+)\s+(\d+)" [ Integer a; Integer b ] -> [ a; b ]
    | _ -> failwith "Parsing input failed"

let parseInput input =
    let lists = input |> String.split '\n' |> List.map getNumbers |> List.transpose

    match lists with
    | [ a; b ] -> a, b
    | _ -> failwith "Couldn't parse input into two lists"

let getDistances a b =
    let zipped = List.zip (List.sort a) (List.sort b)
    zipped |> List.map (fun (a, b) -> a - b |> abs)

let getSimilarity xs ys =
    let yCounts = ys |> getElementCountMap
    let xSimilarity = xs |> List.map (fun x -> x, Map.tryFind x yCounts)

    xSimilarity
    |> List.map (fun (a, b) ->
        a
        * match b with
          | Some count -> count
          | None -> 0)

let private partA input =
    let (a, b) = parseInput input
    let distances = getDistances a b
    distances |> List.sum |> printfn "Total distance: %i"

let private partB input =
    let (a, b) = parseInput input
    let similarities = getSimilarity a b
    similarities |> List.sum |> printfn "Similarity score: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
