module AOC.Year2016.Day9

open AOC
open System.Text.RegularExpressions

let (|ParseRegexi|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let pattern = @"\((\d+)x(\d+)\)"

let decompress (value: string) : string =
    let rec loop (output: string) index =
        let remaining = value[index..]
        let y = Regex.Match(remaining, pattern)

        if y.Success then
            let a = int y.Groups[1].Value
            let b = int y.Groups[2].Value

            let j = y.Index + y.Length
            let beforeInsert = remaining[.. y.Index - 1]
            let insert = String.replicate b remaining[j .. j + a - 1]

            loop (output + beforeInsert + insert) (index + j + a)
        else
            output + remaining

    loop "" 0

type Node =
    | Group of amount: int64 * children: Node list
    | Leaf of length: int64


let parseToNodes (value: string) : Node =
    let rec loop substr nodes =
        let m = Regex.Match(substr, pattern)

        if m.Success then
            let a = int m.Groups[1].Value
            let b = int m.Groups[2].Value
            let x = substr[.. m.Index - 1]
            let y = substr[m.Index + m.Length .. m.Index + m.Length - 1 + a]
            let z = substr[m.Index + m.Length + a ..]
            nodes @ [ Leaf(x.Length); Group(b, loop y []) ] @ loop z []
        else
            nodes @ [ Leaf(substr.Length) ]

    Group(1, loop value [])

let getNodeLength node =
    let rec loop node =
        match node with
        | Leaf(length = l) -> l
        | Group(amount = a; children = c) -> a * (List.sumBy loop c)

    loop node

let partA = decompress >> String.length >> printfn "%i"

let partB = parseToNodes >> getNodeLength >> printfn "%i"

let solution: Types.Solution = { partA = partA; partB = partB }
