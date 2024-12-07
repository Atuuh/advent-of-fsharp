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

let step grid guardDirection guardPosition =
    let nextPosition = add guardPosition (getVectorFromDirection (guardDirection))

    match tryGetItem grid nextPosition with
    | Some Obstacle ->
        let rotatedGuard = rotateGuard guardDirection
        Some(rotatedGuard, guardPosition)
    | Some _ -> Some(guardDirection, nextPosition)
    | None -> None

open System.Collections.Generic

type Patrol =
    | Loop of HashSet<Point * Direction>
    | Exits of HashSet<Point * Direction>

let isLoop (patrol: Patrol) = patrol.IsLoop

let patrol startingPosition startingDirection grid =
    let visited = HashSet<Point * Direction>()
    visited.Add(startingPosition, startingDirection) |> ignore

    let rec loop currentPosition currentDirection =
        match step grid currentDirection currentPosition, visited with
        | Some(direction, position), visited when visited.Contains(position, direction) -> Loop visited
        | Some(direction, position), _ ->
            visited.Add(position, direction) |> ignore
            loop position direction
        | None, _ -> Exits visited

    loop startingPosition startingDirection

let getAllSteps patrol =
    match patrol with
    | Loop xs -> xs
    | Exits xs -> xs
    |> seq
    |> Seq.map fst

let private partA input =
    let grid, guardPosition, guardDirection = parseInput input
    let allSteps = patrol guardPosition guardDirection grid |> getAllSteps
    let answer = allSteps |> Seq.length
    printfn "Answer: %i" answer

let private partB input =
    let grid, guardPosition, guardDirection = parseInput input

    let originalPath =
        patrol guardPosition guardDirection grid
        |> getAllSteps
        |> Seq.filter ((pointEqual guardPosition) >> not)

    let newGrids = originalPath |> Seq.map (fun point -> set2d grid point Obstacle)

    let answer =
        newGrids
        |> Seq.map (fun grid -> async { return patrol guardPosition guardDirection grid |> isLoop })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.filter (id)
        |> Seq.length

    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
