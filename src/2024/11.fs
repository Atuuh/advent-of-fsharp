module AOC.Year2024.Day11

open AOC

let parseInput =
    String.splits " "
    >> List.map uint64
    >> List.countBy id
    >> List.map (fun (a, b) -> a, uint64 b)
    >> Map.ofList

let canSplitStone stone = String.length (string stone) % 2 = 0

let splitStone stone =
    match stone with
    | 0UL -> [ 1UL ]
    | s when canSplitStone s ->
        let stoneString = string s
        let splitIndex = String.length stoneString / 2

        try
            [ (stoneString[.. (splitIndex - 1)]) |> uint64
              (stoneString[splitIndex..]) |> uint64 ]
        with _ ->
            failwithf "failed splitting stone %i %s" s stoneString
    | s -> [ s * 2024UL ]

let blink (stones: Map<uint64, uint64>) times =
    let rec updateStones (remainingStones: (uint64 * uint64) list) (result: Map<uint64, uint64>) =
        match remainingStones with
        | [] -> result
        | (k, v) :: tail ->
            let updatedStones =
                splitStone k
                |> List.fold
                    (fun r item ->
                        r
                        |> Map.change item (fun l ->
                            match l with
                            | Some m -> Some(m + v)
                            | None -> Some v))
                    result

            updateStones tail updatedStones

    let rec loop s t : Map<uint64, uint64> =
        match t with
        | 0 -> s
        | _ -> loop (updateStones (Map.toList s) Map.empty) (t - 1)

    loop stones times

let private partA input =
    let stones = parseInput input
    let updatedStones = blink stones 25
    let answer = updatedStones |> Map.toList |> List.sumBy snd
    printfn "Answer: %i" answer

let private partB input =
    let stones = parseInput input
    let updatedStones = blink stones 75
    let answer = updatedStones |> Map.toList |> List.sumBy snd
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
