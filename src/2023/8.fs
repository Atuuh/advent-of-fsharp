module AOC.Year2023.Day8

open AOC
open AOC.String
open System.Text.RegularExpressions

let rec cycle xs =
    seq {
        yield! xs
        yield! cycle xs
    }

type Node = string * (string * string)

let parseInput (value: string) =
    let parts = value.Split("\n\n")
    let directions = Seq.head parts |> (splitEmpty >> cycle)

    let map =
        (Seq.item 1 parts)
        |> split '\n'
        |> Seq.map (fun line ->
            match line with
            | ParseRegex "(\w+) = \((\w+), (\w+)\)" [ a; b; c ] -> a, (b, c)
            | _ -> raise (System.ArgumentException $"Bad input: {line}"))

    directions, (Map.ofSeq map)

let search map directions startingPosition =
    let searchMap position = Map.find position map

    directions
    |> Seq.scan
        (fun oldPositions direction ->
            oldPositions
            |> (fun (_, (left, right)) ->
                match direction with
                | "L" -> left, searchMap left
                | "R" -> right, searchMap right))
        startingPosition

let private partA =
    parseInput
    >> fun (directions, map) -> search map directions ("AAA", Map.find "AAA" map)
    >> Seq.takeWhile (fun (x, _) -> x <> "ZZZ")
    >> Seq.length
    >> printfn "Result: %A"

let primeDecomposition value =
    let rec loop n x xs =
        if n = x then x :: xs
        else if n % x = 0 then loop (n / x) x (x :: xs)
        else loop n (x + 1) xs

    if value < 2 then List.empty else loop value 2 List.empty

let lowestCommonMultiple =
    List.collect (primeDecomposition >> List.map uint64 >> List.groupBy id)
    >> List.groupBy fst
    >> List.collect (snd >> List.maxBy (snd >> List.length) >> snd)
    >> List.reduce (*)

let private partB =
    parseInput
    >> (fun (directions, map) ->
        let startingPositions =
            map |> Map.filter (fun x _ -> Regex("..A").IsMatch(x)) |> Map.toList

        startingPositions |> List.map (search map directions))
    >> List.map (Seq.takeWhile (fun (x, _) -> (not <| Regex("..Z").IsMatch(x))) >> Seq.length)
    >> lowestCommonMultiple
    >> printfn "Result: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
