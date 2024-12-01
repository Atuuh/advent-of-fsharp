module AOC.Year2023.Day13

open AOC

let parseInput (value: string) =
    value.Split("\n\n")
    |> Seq.map (String.split '\n' >> List.map String.splitEmpty)
    |> List.ofSeq

let compare xs ys =
    let slice = List.truncate (min (List.length xs) (List.length ys))
    List.forall2 (=) (slice xs) (slice ys)

let findPatternIndex a =
    let rec loop remaining lookedAt i =
        match remaining with
        | head :: tail ->
            if List.isEmpty tail then None
            elif compare (head :: lookedAt) tail then Some(i + 1)
            else loop tail (head :: lookedAt) (i + 1)
        | [] -> None

    loop a [] 0

let private partA =
    parseInput
    >> List.map (fun grid ->
        let rowIndex = findPatternIndex grid
        let colIndex = (List.transpose >> findPatternIndex) grid
        rowIndex, colIndex)
    >> List.map (fun (a, b) ->
        match a, b with
        | Some x, _ -> x * 100
        | _, Some x -> x
        | _ -> raise (System.ArgumentException "One grid has no reflections"))
    >> List.sum
    >> printfn "Result: %A"

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
