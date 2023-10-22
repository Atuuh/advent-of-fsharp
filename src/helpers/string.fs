module AOC.String

let uppercase (value: string) = value.ToUpper()

let split (separator: char) (value: string) =
    value.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
