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
        point.X >= 0 && point.X < width  && point.Y >= 0 && point.Y < height 

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

                let [a;b;c;d] = words


                let test t = List.contains "AM" t && List.contains "AS" t
                let passes = test [a;c] && test [b;d]

                if passes then
                    printfn "a=%s b=%s c=%s d=%s" a b c d
                    printfn "\n%c %c\n %s \n%c %c" (d.Chars 1) (a.Chars 1) "A" (c.Chars 1) (b.Chars 1) 

                passes :: res


            else
                res)
        ([])
        |> List.filter ((=) true)

let private partA input =
    let grid = parseInput input
    let matchingWords = getAllMatchingWords grid allDirections "XMAS"
    let answer = matchingWords |> List.length
    printfn "Answer: %i" answer

let private partB input =
    let grid = parseInput input
    let matchingWords = getAllMatchingWords2 grid allDirections "XMAS"
    printfn "Matching words = %A" matchingWords
    let answer = matchingWords |> List.length
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
