module AOC.Year2023.Day12

open AOC
open System
open System.Collections.Generic

module private Naive =
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

module private Smart =
    let parseInputRow row =
        match String.split ' ' row with
        | [ springs; records ] -> String.splitEmpty springs, Naive.parsePattern records
        | _ -> raise (ArgumentException $"Parsing input failed: {row}")


    let getValidArrangements springs pattern =
        let cache = Dictionary<_, int64>()

        let rec loop remainingSprings remainingGroups count needGap =
            let key = remainingSprings, remainingGroups, count, needGap

            match cache.ContainsKey key with
            | true -> cache.Item key
            | false ->
                let result =
                    match remainingSprings, remainingGroups, count, needGap with
                    // reached end of string, no groups remaining, count is 0 => this is a valid grouping
                    | [], [], 0, _ -> 1L
                    // case that spring can be either damaged or operational,
                    | "?" :: _springs, group :: groups, 0, false ->
                        // testing operational, so not incrementing count
                        (loop _springs remainingGroups 0 false)
                        +
                        // testing damaged, so incrementing count
                        (loop _springs groups (group - 1) (group = 1))
                    // case that has to be operational, we have run out of groups and count is zero
                    | "?" :: _springs, [], 0, false -> loop _springs remainingGroups 0 false
                    // Needs gap, must be an operational spring
                    | "?" :: _springs, _, 0, true -> loop _springs remainingGroups 0 false
                    // this is operational, count has to be 0
                    | "." :: _springs, _, 0, _ -> loop _springs remainingGroups 0 false
                    // this is damaged, but count is 0 so we have to take new group. needs gap if next group is 1
                    | "#" :: _springs, group :: groups, 0, false -> loop _springs groups (group - 1) (group = 1)
                    // count isnt 0, so this has to be damaged
                    | "?" :: _springs, _, _count, false -> loop _springs remainingGroups (_count - 1) (_count = 1)
                    // this is damaged and count isnt 0, so decrement and continue
                    | "#" :: _springs, _, _count, false -> loop _springs remainingGroups (_count - 1) (_count = 1)
                    | _ -> 0L

                cache.Add(key, result)
                result

        loop springs pattern 0 false


let private partA input =
    // let rows = input |> String.split '\n' |> List.map Naive.parseInputRow

    // let arrangements =
    //     List.map
    //         (fun (row, pattern) ->
    //             let potentialRows = Naive.getPotentialRows row
    //             let matchingRows = List.filter (Naive.hasDamagePattern pattern) potentialRows
    //             List.length matchingRows)
    //         rows

    // let result = List.sum arrangements
    // printfn $"Amount of possible arrangements: {result}"
    let rows = input |> String.split '\n' |> List.map Smart.parseInputRow

    let arrangements =
        List.map (fun (row, pattern) -> Smart.getValidArrangements row pattern) rows

    let result = List.sum arrangements
    printfn $"Amount of possible arrangements: {result}"


let private partB input =
    let rows = input |> String.split '\n' |> List.map Smart.parseInputRow

    let unfoldedRows =
        List.map
            (fun (springs, (pattern: int list)) ->
                let unfoldedSprings =
                    springs
                    |> List.append [ "?" ]
                    |> List.replicate 4
                    |> List.concat
                    |> List.append springs

                let unfoldedPattern = pattern |> List.replicate 5 |> List.concat
                unfoldedSprings, unfoldedPattern)
            rows

    let arrangements =
        List.map (fun (row, pattern) -> Smart.getValidArrangements row pattern) unfoldedRows

    let result = List.sum arrangements
    printfn $"Amount of possible arrangements: {result}"

let solution: Types.Solution = { partA = partA; partB = partB }
