module AOC.Year2023.Day3

open AOC
open System.Text.RegularExpressions

type Grid = string list list

let logSeq message s =
    printfn "%s" message
    Seq.iter (printfn "%A") s

let getSymbolIndex (grid: Grid) =
    grid
    |> Seq.mapi (fun y row ->
        row
        |> Seq.mapi (fun x char -> if Regex("[0-9\.]").IsMatch char then None else Some(x, y)))
    |> Seq.collect id
    |> Seq.choose id

let private partA =
    Input.mapToList (fun x -> String.splitEmpty x) '\n'
    >> fun grid ->
        let symbolIndexes = getSymbolIndex grid

        logSeq "Symbol indexes" symbolIndexes

        ignore
    >> ignore

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
