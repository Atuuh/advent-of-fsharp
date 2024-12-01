module AOC.Year2016.Day6

open AOC

let solve sortFn =
    String.split '\n'
    >> List.map String.splitEmpty
    >> List.transpose
    >> List.map (List.groupBy id >> sortFn (snd >> List.length) >> List.head >> fst)
    >> String.concat ""
    >> printfn "%s"

let partA = solve List.sortByDescending

let partB = solve List.sortBy

let solution: Types.Solution = { partA = partA; partB = partB }
