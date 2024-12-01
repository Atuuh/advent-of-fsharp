module AOC.Year2016.Day7

open AOC
open System.Text.RegularExpressions

let containsABBA (supernet: string) =
    Seq.zip4 supernet supernet[1..] supernet[2..] supernet[3..]
    |> Seq.exists (fun (a, b, c, d) -> a = d && b = c && a <> b)

let containsABA (hypernets: string list) (supernet: string) =
    Seq.zip3 supernet supernet[1..] supernet[2..]
    |> Seq.exists (fun (a, b, c) -> a = c && a <> b && List.exists (String.contains $"{b}{a}{b}") hypernets)

let splitSequences (value: string) =
    let addresses = Regex.Split(value, @"\[|\]")
    List.foldBack (fun x (l, r) -> x :: r, l) (addresses |> Seq.toList) ([], [])

let supportsTLS =
    splitSequences
    >> (fun (a, b) -> List.exists containsABBA a && not <| List.exists containsABBA b)

let supportsSSL = splitSequences >> (fun (a, b) -> List.exists (containsABA b) a)

let partA =
    Input.toList '\n' >> List.filter supportsTLS >> List.length >> printfn "%A"

let partB =
    Input.toList '\n' >> List.filter supportsSSL >> List.length >> printfn "%A"

let solution: Types.Solution = { partA = partA; partB = partB }
