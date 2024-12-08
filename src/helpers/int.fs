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
