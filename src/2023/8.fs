module AOC.Year2023.Day8

open AOC
open AOC.String

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

let private partA =
    parseInput
    >> (fun (directions, map: Map<string, (string * string)>) ->
        let mutable currentNode = "AAA", Map.find "AAA" map

        let route =
            directions
            |> Seq.map (fun dir ->
                let (currentPos, (left, right)) = currentNode

                currentNode <-
                    match dir with
                    | "L" ->
                        let _node = map |> Map.find left
                        left, _node
                    | "R" ->
                        let _node = map |> Map.find right
                        right, _node

                    | _ -> raise (System.ArgumentException $"Couldn't match node {currentPos}")

                currentNode)
            |> Seq.takeWhile (fun (pos, _) -> pos <> "ZZZ")

        Seq.append (Seq.singleton currentNode) route)

    // >> Seq.iter (printfn "%A")
    >> Seq.length
    >> printfn "Result: %A"

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
