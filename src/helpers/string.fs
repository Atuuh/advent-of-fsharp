module AOC.String

let uppercase (value: string) = value.ToUpper()

let split (separator: char) (value: string) =
    value.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList

let splitEmpty (value: string) =
    value.ToCharArray() |> Seq.map (fun x -> x.ToString()) |> Seq.toList

let contains (substring: string) (value: string) = value.Contains(substring)
