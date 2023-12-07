module AOC.Int

let tryParse (s: string) =
    let mutable intValue = 0L

    if System.Int64.TryParse(s, &intValue) then
        Some(intValue)
    else
        None
