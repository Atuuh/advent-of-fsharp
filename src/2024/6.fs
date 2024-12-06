module AOC.Year2024.Day6

open AOC
open AOC.Point
open AOC.Direction
open AOC.Grid

type Cell =
    | Empty
    | Obstacle

let toCell value =
    match value with
    | "#" -> Obstacle
    | _ -> Empty

let parseInput input =
    let grid = input |> String.splits "\n" |> List.map (String.splitEmpty)
    grid |> map2d toCell, grid |> find ((=) "^"), N

let rotateGuard guard =
    match guard with
    | N -> E
    | E -> S
    | S -> W
    | W -> N
    | _ -> failwith "Guard pretending he can do diagonals"

let step grid (guardDirection, guardPosition) =
    let nextPosition = add guardPosition (getVectorFromDirection (guardDirection))

    match tryGetItem grid nextPosition with
    | Some Obstacle ->
        let rotatedGuard = rotateGuard guardDirection
        Some((rotatedGuard, guardPosition), (rotatedGuard, guardPosition))
    | Some _ -> Some((guardDirection, nextPosition), (guardDirection, nextPosition))
    | None -> None

let getAllSteps guardPosition guardDirection grid =
    (guardDirection, guardPosition) |> Seq.unfold (step grid)

let private partA input =
    let grid, guardPosition, guardDirection = parseInput input
    let allSteps = getAllSteps guardPosition guardDirection grid
    let answer = allSteps |> Seq.map snd |> Seq.distinct |> Seq.length
    printfn "Answer: %i" answer

let private partB input =
    let grid, guardPosition, guardDirection = parseInput input

    let originalPath =
        getAllSteps guardPosition guardDirection grid
        |> Seq.map snd
        |> Seq.distinct
        |> Seq.filter ((pointEqual guardPosition) >> not)

    let count = 10000

    let potentialNewGrids =
        originalPath
        |> Seq.map (fun point ->
            match Day4.getItem grid point with
            // | Empty, pos when not (pointEqual pos guardPosition) -> set2d grid point Obstacle |> Some
            | Empty -> set2d grid point Obstacle |> Some
            | _ -> None)

    let actualNewGrids = potentialNewGrids |> Seq.choose id

    let answer =
        actualNewGrids
        |> Seq.filter (
            getAllSteps guardPosition guardDirection
            >> Seq.truncate count
            >> Seq.length
            >> ((=) count)
        )
        |> Seq.length

    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
