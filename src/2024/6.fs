module AOC.Year2024.Day6

open AOC
open AOC.Point
open AOC.Direction
open AOC.Grid
open System.Diagnostics

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

type Patrol =
    | Loop of Point list
    | Exits of Point list

let isLoop patrol =
    match patrol with
    | Loop _ -> true
    | Exits _ -> false

let patrol startingPosition startingDirection grid =
    let rec loop currentPosition currentDirection (visited: (Point * Direction) Set) =
        match step grid currentDirection currentPosition, visited with
        | Some(direction, position), visited when Set.contains (position, direction) visited ->
            Loop(Set.map fst visited |> Set.toList)
        | Some(direction, position), _ -> loop position direction (visited.Add(position, direction))
        | None, _ -> Exits(Set.map fst visited |> Set.toList)

    loop startingPosition startingDirection (Set.singleton (startingPosition, startingDirection))

let getAllSteps patrol =
    match patrol with
    | Loop xs -> xs
    | Exits xs -> xs

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
        |> Seq.filter (patrol guardPosition guardDirection >> isLoop)
        |> Seq.length

    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
