module AOC.Year2024.Day10

open AOC
open AOC.Direction
open AOC.Grid
open FSharpx.Collections

let bfs (findNeighbours: 'a -> 'a list) greedy (startingPoint: 'a) : 'a list =
    let rec addNeighbours neighbours frontier visited =
        match neighbours with
        | [] -> frontier, visited
        | neighbour :: tail ->
            if greedy || not (List.contains neighbour visited) then
                addNeighbours tail (Queue.conj neighbour frontier) (neighbour :: visited)
            else
                addNeighbours tail frontier visited

    let rec loop (frontier: 'a Queue) (visited: 'a list) =
        match Queue.tryUncons frontier with
        | None -> visited
        | Some(next, tail) ->
            let neighbours = findNeighbours next
            let updatedFrontier, updatedVisited = addNeighbours neighbours tail visited
            loop updatedFrontier updatedVisited

    loop (Queue.conj startingPoint Queue.empty) ([ startingPoint ])

let parseInput input =
    input
    |> String.splits "\n"
    |> List.map (String.splitEmpty >> List.map int)
    |> mapi2d (fun x y value -> ({ Point.Point.X = x; Point.Point.Y = y }), value)

let getNeighbours grid (point, value) =
    cardinal
    |> List.choose (fun direction -> Grid.tryGetItem grid (Point.add point (getVectorFromDirection direction)))
    |> List.filter (fun (_, v) -> v = value + 1)

let private partA input =
    let grid = parseInput input
    let starts = grid |> List.flat |> List.filter (snd >> ((=) 0))
    let visited = starts |> List.map (bfs (getNeighbours grid) false)
    let answer = visited |> List.sumBy (List.filter (snd >> (=) 9) >> List.length)
    printfn "Answer: %i" answer

let private partB input =
    let grid = parseInput input
    let starts = grid |> List.flat |> List.filter (snd >> ((=) 0))
    let visited = starts |> List.map (bfs (getNeighbours grid) true)
    let answer = visited |> List.sumBy (List.filter (snd >> (=) 9) >> List.length)
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
