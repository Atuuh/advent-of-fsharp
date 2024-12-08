module AOC.Year2024.Day4

open AOC
open AOC.Point
open AOC.Grid

type Directions =
    | N
    | E
    | S
    | W
    | NE
    | SE
    | SW
    | NW

let getVectorFromDirection direction =
    match direction with
    | N -> { X = 0; Y = -1 }
    | NE -> { X = 1; Y = -1 }
    | E -> { X = 1; Y = 0 }
    | SE -> { X = 1; Y = 1 }
    | S -> { X = 0; Y = 1 }
    | SW -> { X = -1; Y = 1 }
    | W -> { X = -1; Y = 0 }
    | NW -> { X = -1; Y = -1 }

let getPoints grid direction length startingPosition =
    let vector = getVectorFromDirection direction
    let points = [ for x in 0 .. length - 1 -> startingPosition |> add (mult vector x) ]

    if List.forall (withinBounds grid) points then
        Some points
    else
        None

let getItem grid { X = x; Y = y } = grid |> List.item y |> List.item x

let parseInput input =
    input |> String.split '\n' |> List.map String.splitEmpty

let getAllGridItems grid =
    grid
    |> List.mapi (fun y row -> row |> List.mapi (fun x item -> { X = x; Y = y }, item))
    |> List.flat

let allDirections = [ N; NE; E; SE; S; SW; W; NW ]

let getAllXmas grid =
    grid
    |> getAllGridItems
    |> List.fold
        (fun acc (point, value) ->
            if value = "X" then
                let words =
                    allDirections
                    |> List.map (fun direction -> getPoints grid direction 4 point)
                    |> List.map (Option.map (List.map (getItem grid) >> String.concat ""))

                let passingWords =
                    words |> List.choose id |> List.filter ((=) "XMAS") |> List.length

                passingWords + acc
            else
                acc)
        0

// a b
//  A
// c d
let hasCrossPattern (grid: string list list) point =
    let items =
        [ NW; NE; SW; SE ]
        |> List.map (fun direction -> getItem grid (add point (getVectorFromDirection direction)))

    match items with
    | [ a; b; c; d ] -> [ a + d; b + c ] |> List.forall (fun value -> value = "SM" || value = "MS")
    | _ -> failwith "Impossible"

let private partA input =
    let grid = parseInput input
    let answer = getAllXmas grid
    printfn "Answer: %i" answer

let private partB input =
    let grid = parseInput input
    let height = List.length grid
    let width = List.length (List.head grid)

    let isCrossPatternCenter point =
        (getItem grid point = "A") && (hasCrossPattern grid point)

    let answer =
        [ for j in 1 .. height - 2 do
              for i in 1 .. width - 2 -> { X = i; Y = j } ]
        |> List.filter (isCrossPatternCenter)
        |> List.length

    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
