module AOC.Year2016.Day1

open AOC
open AOC.List

exception private Error of string

let private test (fn: 'x -> 'y) (x: 'x) (y: 'y) =
    let result = fn x

    if result <> y then
        raise (Error($"Test failed. fn x -> {result} does not match expected result {y}"))

let private getManhattanDistance (x: int, y: int) : int = abs x + abs y

test getManhattanDistance (3, 2) 5
test getManhattanDistance (0, -2) 2
test getManhattanDistance (10, 2) 12

type private Position = { x: int; y: int; direction: char }
type private Direction = char
type private Move = { direction: Direction; distance: int }

let private moveFrom (position: Position) =
    match position.direction with
    | 'N' ->
        { x = position.x
          y = position.y + 1
          direction = position.direction }
    | 'E' ->
        { x = position.x + 1
          y = position.y
          direction = position.direction }
    | 'S' ->
        { x = position.x
          y = position.y - 1
          direction = position.direction }
    | 'W' ->
        { x = position.x - 1
          y = position.y
          direction = position.direction }
    | _ -> raise (Error("Direction is fucked"))

let private turn direction (position: Position) =
    match position.direction with
    | 'N' when direction = 'L' -> 'W'
    | 'N' when direction = 'R' -> 'E'
    | 'E' when direction = 'L' -> 'N'
    | 'E' when direction = 'R' -> 'S'
    | 'S' when direction = 'L' -> 'E'
    | 'S' when direction = 'R' -> 'W'
    | 'W' when direction = 'L' -> 'S'
    | 'W' when direction = 'R' -> 'N'
    | _ -> raise (Error("Direction is fucked"))


let private doMove (position: Position list) (move: Move) : Position list =
    let lastPosition = List.last position
    let newDirection = turn move.direction lastPosition

    let turnedPosition =
        { x = lastPosition.x
          y = lastPosition.y
          direction = newDirection }

    let rec loop moves (currentPosition: Position) amount =
        if amount = 0 then
            List.tail moves
        else
            let newPosition = moveFrom currentPosition
            loop (moves @ [ newPosition ]) newPosition (amount - 1)

    loop [ turnedPosition ] turnedPosition move.distance

let private log a =
    printfn "%A" a
    a

let private getFirstDuplicate (comparisonFn: 'a -> 'a -> bool) (list: 'a list) =
    let rec checkList visitedItems remainingItems =
        match remainingItems with
        | head :: tail ->
            if List.exists (comparisonFn head) visitedItems then
                Some(head)
            else
                checkList (visitedItems @ [ head ]) (tail)
        | [] -> None

    checkList List.empty list

let private positionsEqual a b = a.x = b.x && a.y = b.y

let private strToMove (str: string) =
    { direction = str[0]
      distance = str[1..] |> int }

let private initialPosition = { x = 0; y = 0; direction = 'N' }

let private output distance =
    match distance with
    | Some x -> printfn $"Bunny HQ is {x} blocks away"
    | None -> printfn "Could not find bunny hq!"

let private partA =
    Input.mapToList strToMove ','
    >> List.scan doMove [ initialPosition ]
    >> flat
    >> List.tryLast
    >> Option.map (fun position -> (position.x, position.y))
    >> Option.map getManhattanDistance
    >> output

let private partB =
    Input.mapToList strToMove ','
    >> List.scan doMove [ initialPosition ]
    >> flat
    >> getFirstDuplicate positionsEqual
    >> Option.map (fun position -> (position.x, position.y))
    >> Option.map getManhattanDistance
    >> output

let solution: Types.Solution = { partA = partA; partB = partB }
