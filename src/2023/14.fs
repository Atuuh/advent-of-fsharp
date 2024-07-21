module AOC.Year2023.Day14

open AOC
open AOC.List
open System.Collections.Generic

type Item =
    | Round
    | Cube
    | Empty

let tiltColumn (items: Item list) : Item list =
    let folder (x: Item) (xs: Item list) : Item list =
        match xs, x with
        | Round :: ys, Empty -> Round :: (Empty :: ys)
        | _ -> x :: xs

    let rec loop (xs: Item list) : Item list =
        let sorted = List.foldBack folder xs []

        match sorted = xs with
        | true -> sorted
        | false -> loop sorted

    loop items

let tilt items = List.map tiltColumn items

let toItem value =
    match value with
    | "O" -> Round
    | "#" -> Cube
    | "." -> Empty
    | _ -> raise <| invalidArg (nameof value) (sprintf "Invalid value to parse: %A" value)

let parseInput input =
    input |> String.split '\n' |> List.map (String.splitEmpty >> List.map toItem)

let getLoad row n =
    List.sumBy
        (fun item ->
            match item with
            | Round -> n
            | _ -> 0)
        row

let rotate items = items |> List.map List.rev |> transpose

let printItems msg items =
    let printItem item =
        match item with
        | Round -> "O"
        | Empty -> "."
        | Cube -> "#"

    items
    |> transpose
    |> List.fold (fun result row -> result + (row |> List.map printItem |> String.concat "") + "\n") ""
    |> printfn "%s\n%s" msg


let spin items =
    items |> tilt |> rotate |> tilt |> rotate |> tilt |> rotate |> tilt |> rotate

let spinMultiple n items =
    let history = Dictionary<Item list list, int>()

    let rec loop xs c =
        let spun = spin xs

        match history.ContainsKey spun with
        | true ->
            let loopIndex = history.Item spun
            let loopLength = c - loopIndex
            let endIndex = ((n - c) % loopLength) + loopIndex - 1
            let finalEntry = history |> Seq.find (fun x -> x.Value = endIndex)
            finalEntry.Key
        | false ->
            history.Add(spun, c)

            match c, spun = xs with
            | x, _ when (x + 1) = n -> spun
            | _, true -> spun
            | _, false -> loop spun (c + 1)

    history.Add(items, 0)
    loop items 0

let private partA input =
    let items = parseInput input
    let tilted = List.map tiltColumn (transpose items) |> transpose

    let totalLoad =
        tilted
        |> List.mapi (fun n row -> getLoad row (List.length tilted - n))
        |> List.sum

    printfn "Total load is %i" totalLoad

let private partB input =
    let items = parseInput input |> transpose
    let spun = spinMultiple 1000000000 items |> transpose

    let totalLoad =
        spun |> List.mapi (fun n row -> getLoad row (List.length spun - n)) |> List.sum

    printfn "Total load is %i" totalLoad


let solution: Types.Solution = { partA = partA; partB = partB }
