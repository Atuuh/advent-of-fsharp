module AOC.Grid

open AOC.Point

type Grid<'a> = 'a list list

let find predicate (grid: 'a Grid) =
    grid
    |> List.findIndex (fun row -> List.exists predicate row)
    |> (fun y ->
        let x = List.item y grid |> List.findIndex predicate
        { X = x; Y = y })

let map2d mapping grid = grid |> List.map (List.map mapping)

let set2d (grid: 'a list list) { X = x; Y = y } (value: 'a) : 'a list list =
    List.concat
        [ grid[.. y - 1]
          [ List.concat [ grid[y][.. x - 1]; [ value ]; grid[y][x + 1 ..] ] ]
          grid[y + 1 ..] ]

let tryGetItem grid point =
    try
        grid |> List.item point.Y |> List.item point.X |> Some
    with _ ->
        None

let height grid = List.length grid
let width grid = grid |> List.item 0 |> List.length
