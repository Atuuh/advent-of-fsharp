module AOC.Year2023.Day10

open AOC

let parseInput = String.split '\n' >> List.map String.splitEmpty >> array2D //fun grid -> Array2D.init (List.length grid) (grid |> List.head >> List.length)

let find2D (needle: 'a) (haystack: 'a[,]) =
    let rec loop x y =
        if x >= haystack.GetLength 1 then loop 0 (y + 1)
        elif haystack.[y, x] = needle then x, y
        else loop (x + 1) y

    loop 0 0


let getAnimalPosition grid = find2D "S" grid

let lefts = [ "-"; "L"; "F" ]
let rights = [ "-"; "J"; "7" ]
let ups = [ "|"; "7"; "F" ]
let downs = [ "|"; "L"; "J" ]
let isConnected pipe pipes = pipe = "S" || List.contains pipe pipes

let getNormalNeighbours grid (x, y) =
    let mutable xs = []

    if x > 0 then
        xs <- ((x - 1, y) :: xs)

    if y > 0 then
        xs <- ((x, y - 1) :: xs)

    if x < Array2D.length2 grid - 1 then
        xs <- ((x + 1, y) :: xs)

    if y < Array2D.length1 grid - 1 then
        xs <- ((x, y + 1) :: xs)

    xs

let getNeighbours grid (x, y) =
    let mutable xs = []
    let pipe = Array2D.get grid y x

    if x > 0 && isConnected pipe rights then
        if isConnected (Array2D.get grid y (x - 1)) lefts then
            xs <- ((x - 1, y) :: xs)

    if y > 0 && isConnected pipe downs then
        if isConnected (Array2D.get grid (y - 1) x) ups then
            xs <- ((x, y - 1) :: xs)

    if x < Array2D.length2 grid - 1 && isConnected pipe lefts then
        if isConnected (Array2D.get grid y (x + 1)) rights then
            xs <- ((x + 1, y) :: xs)

    if y < Array2D.length1 grid - 1 && isConnected pipe ups then
        if isConnected (Array2D.get grid (y + 1) x) downs then
            xs <- ((x, y + 1) :: xs)

    xs

let getPrintChar pipe =
    match pipe with
    | "-" -> "═"
    | "|" -> "║"
    | "L" -> "╚"
    | "J" -> "╝"
    | "F" -> "╔"
    | "7" -> "╗"
    | _ -> pipe

let printFilteredGrid grid pipes =
    Array2D.iteri
        (fun y x char ->
            if List.exists ((=) (x, y)) pipes then
                printf "%s" (getPrintChar char)
            else if Array2D.get grid y x = "." then
                printf "."
            else
                printf " "

            if x = Array2D.length2 grid - 1 then
                printf "\n")
        grid

let printGrid grid =
    Array2D.iteri
        (fun y x char ->
            printf "%s" (getPrintChar char)

            if x = Array2D.length2 grid - 1 then
                printf "\n")
        grid

let getWholePipe grid startingPosition =
    let rec loop newSections allPipes =
        if List.isEmpty newSections then
            allPipes
        else
            let neighbours = List.collect (getNeighbours grid) newSections |> List.distinct
            let g = List.except allPipes neighbours
            loop g (allPipes @ g)

    loop [ startingPosition ] [ startingPosition ]


let private partA =
    parseInput
    >> (fun grid ->
        let animalPosition = getAnimalPosition grid

        let allPipes = getWholePipe grid animalPosition
        printFilteredGrid grid allPipes
        List.length allPipes / 2)
    >> printfn "Result: %A"

let flatten grid =
    Array2D.mapi (fun y x char -> (x, y, char)) grid

let expandGrid grid =
    let mutable newGrid =
        Array2D.create (Array2D.length1 grid * 2 - 1) (Array2D.length2 grid * 2 - 1) " "

    Array2D.iteri
        (fun y x pipe ->
            Array2D.set newGrid ((y * 2)) (x * 2) pipe

            if y <> Array2D.length1 grid - 1 && isConnected pipe ups then
                Array2D.set newGrid (y * 2 + 1) (x * 2) "|"

            if x <> Array2D.length2 grid - 1 && isConnected pipe lefts then
                Array2D.set newGrid (y * 2) (x * 2 + 1) "-"

        )
        grid

    newGrid

let shrinkGrid grid =
    let newGrid =
        Array2D.create ((Array2D.length1 grid + 1) / 2) ((Array2D.length2 grid + 1) / 2) " "

    Array2D.iteri
        (fun y x tile ->
            if x % 2 = 0 || y % 2 = 0 then
                Array2D.set newGrid (y / 2) (x / 2) tile)
        grid

    newGrid

let getGroundTiles grid =
    let mutable xs = []

    Array2D.iteri
        (fun y x tile ->
            if tile = "." then
                xs <- (x, y) :: xs)
        grid

    xs

let floodfill grid position banned =
    let rec loop neighbours tiles =
        if List.isEmpty neighbours then
            printfn "Floodfill for pos %A found friends %A" position tiles
            tiles
        else
            let newNeighbours =
                neighbours
                |> List.collect (getNormalNeighbours grid)
                |> List.filter (fun (x, y) ->
                    let tile = Array2D.get grid y x
                    tile = "." || tile = " ")
                |> List.filter (fun (x, y) -> not (List.contains (x, y) banned))
                |> List.except tiles

            loop newNeighbours (tiles @ newNeighbours)

    loop [ position ] [ position ]

let getEnclosedGround grid groundTiles pipeLoop =
    let rec loop (positions) enclosed =
        if List.isEmpty positions then
            enclosed
        else
            let group = floodfill grid (List.head positions) pipeLoop
            printfn "Ground group"
            printfn "%A" group

            let pretty =
                Array2D.mapi (fun y x pipe -> if List.contains (x, y) group then "■" else pipe) grid

            printGrid pretty
            let filtered = List.except (group @ [ List.head positions ]) (List.tail positions)

            if
                List.forall
                    (fun (x, y) ->
                        x <> 0
                        && x <> Array2D.length2 grid - 1
                        && y <> 0
                        && y <> Array2D.length1 grid - 1)
                    group
            then
                loop filtered (enclosed @ group)
            else
                loop filtered enclosed


    printfn "Ground Tiles"
    printfn "%A" groundTiles
    printfn "Length x: %i" <| Array2D.length2 grid
    printfn "Length y: %i" <| Array2D.length1 grid
    loop groundTiles []

let getNonLoopTiles grid loop =
    let mutable xs = []

    Array2D.iteri
        (fun y x tile ->
            if not (List.contains (x, y) loop) then
                xs <- (x, y) :: xs)
        grid

    xs

let scaleTiles = List.map (fun (x, y) -> x * 2, y * 2)

let x grid =
    let mutable xs = []

    Array2D.iteri
        (fun y x tile ->
            if tile <> "." && tile <> " " then
                xs <- (x, y) :: xs)
        grid

    xs

let private partB =
    parseInput
    >> (fun grid ->
        let animalPosition = getAnimalPosition grid
        let allPipes = getWholePipe grid animalPosition
        printfn "Starting"
        printFilteredGrid grid allPipes
        printfn "All Pipes: %A" allPipes

        let filteredGrid =
            Array2D.mapi
                (fun y x pipe ->
                    if List.contains (x, y) allPipes || pipe = "." then
                        pipe
                    else
                        " ")
                grid

        printfn "Filtered"
        printFilteredGrid filteredGrid allPipes

        let expandedGrid = expandGrid filteredGrid
        printfn "Expanded"
        printGrid expandedGrid

        let groundTiles = getNonLoopTiles grid allPipes |> scaleTiles

        let enclosed = getEnclosedGround expandedGrid groundTiles (x expandedGrid)
        printfn "Enclosed ground tiles"
        printfn "%A" enclosed

        let pretty =
            Array2D.mapi (fun y x pipe -> if List.contains (x, y) enclosed then "■" else pipe) expandedGrid

        printGrid pretty

        enclosed
        |> List.filter (fun x -> List.exists (fun y -> x = y) groundTiles)
        |> List.length

    )
    >> printfn "Result: %A"

let solution: Types.Solution = { partA = partA; partB = partB }
