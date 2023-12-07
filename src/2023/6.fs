module AOC.Year2023.Day6

open AOC
open AOC.String

let parseCorrect =
    split ':' >> List.item 1 >> split ' ' >> (List.choose Int.tryParse)

let parseBadKerning =
    split ':'
    >> List.item 1
    >> splitEmpty
    >> List.where (fun x -> Int.tryParse x |> Option.isSome)
    >> String.concat ""
    >> int64
    >> List.singleton

let parseInput numberMapFn = split '\n' >> List.map numberMapFn

let getDistancesFor raceLength =
    [ raceLength / 2L .. -1L .. 0L ] |> List.map (fun n -> n * (raceLength - n))

let isOdd n = (n &&& 1L) = 1L

let getBetterDistances =
    List.transpose
    >> List.map (fun [ time; distance ] ->
        time
        |> (getDistancesFor
            >> List.takeWhile ((<) distance)
            >> List.length
            >> fun n -> (n * 2) - if isOdd time then 0 else 1))

let private partA =
    parseInput parseCorrect
    >> getBetterDistances
    >> List.reduce (*)
    >> printfn "Answer: %i"

let private partB =
    parseInput parseBadKerning
    >> getBetterDistances
    >> List.reduce (*)
    >> printfn "Answer: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
