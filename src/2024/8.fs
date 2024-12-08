module AOC.Year2024.Day8

open AOC
open AOC.Point
open AOC.Grid
open AOC.Int

let combinations xs =
    let rec loop remaining result =
        match remaining, result with
        | [], _ -> result
        | head :: tail, _ -> loop tail (List.concat [ List.allPairs [ head ] tail; result ])

    loop xs []

let rec nodes grid point vector =
    if withinBounds grid point then
        point :: nodes grid (add point vector) vector
    else
        []

let antinodes grid repeating points =
    combinations points
    |> List.collect (fun (a, b) ->
        let vector = subtract b a

        if repeating then
            List.concat [ [ a; b ]; nodes grid b vector; nodes grid a (mult vector -1) ]
        else
            [ add b vector; subtract a vector ])

let parseInput input =
    let grid = input |> Input.mapToList (String.splitEmpty) '\n'

    let points =
        grid
        |> Grid.mapi2d (fun x y item -> { X = x; Y = y }, item)
        |> List.flat
        |> List.filter (snd >> (<>) ".")

    let grouped =
        points
        |> List.groupBy snd
        |> List.map (fun (a, points) -> a, List.map fst points)

    grid, grouped

let private partA input =
    let grid, antennaPositions = parseInput input

    let antinodes =
        antennaPositions |> List.map (snd >> antinodes grid false) |> List.flat

    let answer =
        antinodes |> List.filter (withinBounds grid) |> List.distinct |> List.length

    printfn "Answer %i" answer

printfn "Decomp %i = %A" 30 (factorise 30)
printfn "Decomp %i = %A" 1 (factorise 1)
printfn "Decomp %i = %A" 3 (factorise 3)
printfn "Decomp %i = %A" 80 (factorise 80)

let private partB input =
    let grid, antennaPositions = parseInput input

    let antinodes =
        antennaPositions |> List.map (snd >> antinodes grid true) |> List.flat

    let answer = antinodes |> List.filter (withinBounds grid) |> List.distinct
    printfn "Answer %i" (answer |> List.length)

let solution: Types.Solution = { partA = partA; partB = partB }
