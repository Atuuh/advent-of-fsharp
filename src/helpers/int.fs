module AOC.Int

let tryParse (s: string) =
    let mutable intValue = 0L

    if System.Int64.TryParse(s, &intValue) then
        Some(intValue)
    else
        None

let inline factorise value =
    let rec loop x i results =
        match x with
        | x when x < 2 -> results
        | x when x % i = 0 -> loop (x / i) (i) (i :: results)
        | _ -> loop x (i + 1) results

    loop value 2 []

let inline lowestCommonMultiple xs =
    let factors = xs |> List.map factorise
    let distinctFactors = factors |> List.flat |> List.distinct
    let f = factors |> List.map (fun facts -> facts |> List.countBy id |> Map.ofList)

    distinctFactors
    |> List.fold
        (fun total prime ->
            let maxCount =
                f
                |> List.map (fun ys ->
                    match Map.tryFind prime ys with
                    | Some x -> x
                    | None -> 0)
                |> List.max

            pown prime maxCount * total)
        1
