module AOC.Year2023.Day11

open AOC
open AOC.Tuple

let parseInput =
    String.split '\n'
    >> List.map String.splitEmpty
    >> List.mapi (fun y rows -> rows |> List.mapi (fun x char -> (x, y, char)))
    >> List.flat

let getGaps xs =
    seq { 0..1 .. Seq.max xs } |> Seq.except xs

let getInternalPairs (xs: 'a list) =
    List.mapi (fun i x -> List.allPairs (xs[i + 1 ..]) ([ x ])) xs |> List.flat

let inline getManhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let private partA =
    parseInput
    >> List.filter (fun (x, y, char) -> char = "#")
    >> fun galaxies ->
        let colGaps = getGaps (List.map fst3 galaxies)
        let rowGaps = getGaps (List.map snd3 galaxies)

        List.map
            (fun (x, y, char) ->
                x + (Seq.filter ((>) x) colGaps |> Seq.length), y + (Seq.filter ((>) y) rowGaps |> Seq.length))
            galaxies
    >> getInternalPairs
    >> List.map (fun (a, b) -> getManhattanDistance a b)
    >> List.sum
    >> printfn "Result: %A"

let getOffset x = if x > 0L then 1L else 0L

let private partB =
    parseInput
    >> List.filter (fun (x, y, char) -> char = "#")
    >> fun galaxies ->
        let colGaps = getGaps (List.map fst3 galaxies)
        let rowGaps = getGaps (List.map snd3 galaxies)
        let scale = 1_000_000L

        List.map
            (fun (x, y, char) ->
                let colGapsBetween = int64 (Seq.filter ((>) x) colGaps |> Seq.length)
                let rowGapsBetween = int64 (Seq.filter ((>) y) rowGaps |> Seq.length)

                int64 x + (scale - getOffset colGapsBetween) * colGapsBetween,
                int64 y + (scale - getOffset rowGapsBetween) * rowGapsBetween)

            galaxies
    >> getInternalPairs
    >> List.map (fun (a, b) -> getManhattanDistance a b)
    >> List.sum
    >> printfn "Result: %i"

let solution: Types.Solution = { partA = partA; partB = partB }



(*
    Gaps at columns 2, 5, 8
    Gaps at rows 3, 7 
    3,0 -> 4,0
    7,1 -> 9,1
    0,2 -> 0,2
    6,4 -> 8,5 


...#......
............#



*)
