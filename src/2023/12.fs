module AOC.Year2023.Day12

open AOC
open System


type Spring =
    | Damaged of string
    | Operational of string
    | Unknown of string

let operational a = Operational a


let row: Spring list = []

let groupBySequence list =
    List.fold
        (fun groups item ->
            match groups with
            | [] -> [ item, 1 ]
            | (head, count) :: tail ->
                match head = item with
                | true -> (head, count + 1) :: tail
                | false -> (item, 1) :: groups)
        []
        list
    |> List.rev

let getPotentialRows (row: Spring list) =
    let rec loop rows i =
        match List.length row = i with
        | true -> rows
        | false ->
            let spring = List.item i row

            match spring with
            | Damaged _
            | Operational _ -> loop rows (i + 1)
            | Unknown _ ->
                let newRows =
                    List.collect
                        (fun (_row: Spring list) ->
                            let front = _row[.. (i - 1)]
                            let back = _row[(i + 1) ..]
                            [ front @ Operational "." :: back; front @ Damaged "#" :: back ])
                        rows

                loop newRows (i + 1)

    loop [ row ] 0

let hasDamagePattern pattern row =
    groupBySequence row
    |> List.filter (fun (spring, _) ->
        match spring with
        | Damaged _ -> true
        | _ -> false)
    |> List.map snd
    |> (=) pattern

let parseSpring s =
    match s with
    | "#" -> Damaged s
    | "." -> Operational s
    | "?" -> Unknown s
    | _ -> raise (ArgumentException $"Parsing spring with invalid value: {s}")

let parsePattern = String.split ',' >> List.map int

let parseInputRow row =
    match String.split ' ' row with
    | [ springs; records ] ->
        let actualSprings = String.splitEmpty springs
        List.map parseSpring actualSprings, parsePattern records
    | _ -> raise (ArgumentException $"Parsing input failed: {row}")


let private partA input =
    let rows = input |> String.split '\n' |> List.map parseInputRow

    let arrangements =
        List.map
            (fun (row, pattern) ->
                let potentialRows = getPotentialRows row
                let matchingRows = List.filter (hasDamagePattern pattern) potentialRows
                List.length matchingRows)
            rows

    let result = List.sum arrangements
    printfn $"Amount of possible arrangements: {result}"

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
