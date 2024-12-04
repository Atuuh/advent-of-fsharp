module AOC.String

open System.Text.RegularExpressions

let uppercase (value: string) = value.ToUpper()

let split (separator: char) (value: string) =
    value.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList

let splitEmpty (value: string) =
    value |> Seq.toList |> List.map (fun x -> x.ToString())
// value.ToCharArray() |> Seq.map (fun x -> x.ToString()) |> Seq.toList

let contains (substring: string) (value: string) = value.Contains(substring)

let (|Integer|_|) (str: string) =
    let mutable intvalue = 0

    if System.Int32.TryParse(str, &intvalue) then
        Some(intvalue)
    else
        None

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None
