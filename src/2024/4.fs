module AOC.Year2024.Day4

open AOC

type Point = { X: int; Y: int }

let add (first: Point) (second: Point) =
    { X = first.X + second.X
      Y = first.Y + second.Y }

type CardinalDirections =
    | N
    | E
    | S
    | W

type IntercardinalDirections =
    | NE
    | SE
    | SW
    | NW

type CompassDirections =
    | N
    | E
    | S
    | W
    | NE
    | SE
    | SW
    | NW

let isPointInGrid grid point =
    let height = List.length grid
    let width = List.length (List.head grid)

    let result =
        point.X >= 0 && point.X < width - 1 && point.Y >= 0 && point.Y < height - 1

    result

let getPoints grid direction length startingPosition =
    [ 0 .. length - 1 ]
    |> List.fold
        (fun (res) item ->
            let point =
                match direction with
                | N ->
                    { X = startingPosition.X
                      Y = startingPosition.Y - item }
                | NE ->
                    { X = startingPosition.X + item
                      Y = startingPosition.Y - item }
                | E ->
                    { X = startingPosition.X + item
                      Y = startingPosition.Y }
                | SE ->
                    { X = startingPosition.X + item
                      Y = startingPosition.Y + item }
                | S ->
                    { X = startingPosition.X
                      Y = startingPosition.Y + item }
                | SW ->
                    { X = startingPosition.X - item
                      Y = startingPosition.Y + item }
                | W ->
                    { X = startingPosition.X - item
                      Y = startingPosition.Y }
                | NW ->
                    { X = startingPosition.X - item
                      Y = startingPosition.Y - item }

            if isPointInGrid grid point then point :: res else [])
        ([])
    |> List.rev

let getItem grid { X = x; Y = y } =
    try
        grid |> List.item y |> List.item x
    with :? System.ArgumentException ->
        printfn "Done fucked up, %i,%i" x y
        failwith "Bye"

// let grid: string list list =
//     [ [ "A"; "B"; "C" ]; [ "D"; "E"; "F" ]; [ "G"; "H"; "I" ] ]

// let startingPosition = { X = 1; Y = 1 }
// let points = getPoints grid N 1 startingPosition
// let text = points |> List.map (getItem grid)
// printfn "points test: %A" points
// printfn "text=%A" text


let parseInput input =
    input |> String.split '\n' |> List.map String.splitEmpty

let getAllGridItems grid =
    grid
    |> List.mapi (fun y row -> row |> List.mapi (fun x item -> { X = x; Y = y }, item))
    |> List.flat

let allDirections = [ N; NE; E; SE; S; SW; W; NW ]

let getAllMatchingWords grid directions word =
    grid
    |> getAllGridItems
    |> List.fold
        (fun res (point, value) ->
            if value = "X" then

                let words =
                    allDirections
                    |> List.map (fun direction -> getPoints grid direction 4 point)
                    |> List.map (List.map (getItem grid))
                    |> List.map (String.concat "")


                let matchingWords = words |> List.filter ((=) word)
                List.concat [ matchingWords; res ]


            else
                res)
        ([])

let getAllMatchingWords2 grid directions word =
    grid
    |> getAllGridItems
    |> List.fold
        (fun res (point, value) ->
            if value = "A" then

                let words =
                    [ NE; SE; SW; NW ]
                    |> List.map (fun direction -> getPoints grid direction 2 point)
                    |> List.map (List.map (getItem grid))
                    |> List.map (String.concat "")


                let test [ a; b; c; d ] =
                    List.contains "AM" [ a; c ]
                    && List.contains "AS" [ a; c ]
                    && List.contains "AM" [ b; d ]
                    && List.contains "AS" [ b; d ]

                test words :: res


            else
                res)
        ([])

let private partA input =
    let grid = parseInput input
    printfn "input\n%A" input
    printfn "grid\n%s" (grid |> List.head |> String.concat "")
    let height = List.length grid
    let width = List.length (List.head grid)

    printfn "Height=%i, Width=%i" height width

    printfn "%A" (List.head grid)

    for j = 0 to 0 do
        for i = -1 to width + 1 do
            let point = { X = i; Y = j }
            printfn "Point (%i,%i) is in grid: %b" point.X point.Y (isPointInGrid grid point)

    let matchingWords = getAllMatchingWords grid allDirections "XMAS"
    printfn "Matching words = %A" matchingWords
    let answer = matchingWords |> List.length
    printfn "Answer: %i" answer

let private partB input =
    let grid = parseInput input
    let matchingWords = getAllMatchingWords2 grid allDirections "XMAS"
    printfn "Matching words = %A" matchingWords
    let answer = matchingWords |> List.length
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
