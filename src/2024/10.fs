module AOC.Year2024.Day10

open AOC
open AOC.Direction
open AOC.Grid
open FSharpx.Collections

let bfs (findNeighbours: 'a -> 'a list) (startingPoint: 'a) : 'a list =
    let rec loop (frontier: 'a Queue) (visited: 'a list) =
        match Queue.tryUncons frontier with
        | None -> visited
        | Some(next, tail) ->
            let neighbours = findNeighbours next

            let updatedFrontier, updatedVisited =
                neighbours
                |> List.fold
                    (fun (f, v) neighbour ->
                        if List.contains neighbour v |> not then
                            Queue.conj neighbour f, neighbour :: v
                        else
                            f, v)
                    (tail, visited)

            loop updatedFrontier updatedVisited

    loop (Queue.conj startingPoint Queue.empty) ([ startingPoint ])

let private partA input =
    let grid =
        input
        |> String.splits "\n"
        |> List.map (String.splitEmpty >> List.map int)
        |> mapi2d (fun x y value -> ({ Point.Point.X = x; Point.Point.Y = y }), value)

    let getNeighbours grid (point, value) =
        cardinal
        |> List.choose (fun direction -> Grid.tryGetItem grid (Point.add point (getVectorFromDirection direction)))
        |> List.filter (fun (_, v) -> v = value + 1)

    let starts = grid |> List.flat |> List.filter (snd >> ((=) 0))
    let visited = starts |> List.map (bfs (getNeighbours grid))
    let answer = visited |> List.sumBy (List.filter (snd >> (=) 9) >> List.length)
    printfn "Answer: %i" answer

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
