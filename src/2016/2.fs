module AOC.Year2016.Day2

open AOC.String
open AOC

exception Error of string

type Direction =
    | U
    | D
    | L
    | R

let moveSimple (direction: Direction) (position: int) =
    match direction with
    | L when List.contains position [ 1; 4; 7 ] -> position
    | L -> position - 1
    | R when List.contains position [ 3; 6; 9 ] -> position
    | R -> position + 1
    | U when List.contains position [ 1; 2; 3 ] -> position
    | U -> position - 3
    | D when List.contains position [ 7; 8; 9 ] -> position
    | D -> position + 3

let moveComplex (direction: Direction) (position: int) =
    match direction with
    | L when List.contains position [ 1; 2; 5; 0xA; 0xD ] -> position
    | L -> position - 1
    | R when List.contains position [ 1; 4; 9; 0xC; 0xD ] -> position
    | R -> position + 1
    | U when List.contains position [ 5; 2; 1; 4; 9 ] -> position
    | U -> if position = 3 then 1 else position - 4
    | D when List.contains position [ 5; 0xA; 0xD; 0xC; 9 ] -> position
    | D -> if position = 0xB then 1 else position + 4

let doMoves moveFn startingPosition moves =
    List.fold (fun position nextMove -> moveFn nextMove position) startingPosition moves

let charToDirection char =
    match char with
    | 'L' -> L
    | 'R' -> R
    | 'U' -> U
    | 'D' -> D
    | _ -> raise (Error("Direction is fucked!"))

let private partA =
    Input.mapToList (fun x -> x.ToCharArray() |> Seq.map charToDirection |> Seq.toList) '\n'
    >> List.map (doMoves moveSimple 5)
    >> List.map (sprintf "%i")
    >> String.concat ""
    >> printfn "Code for the toilet is %s"

let private partB =
    Input.mapToList (fun x -> x.ToCharArray() |> Seq.map charToDirection |> Seq.toList) '\n'
    >> List.map (doMoves moveComplex 5)
    >> List.map (sprintf "%x")
    >> String.concat ""
    >> uppercase
    >> printfn "Code for the toilet is %s"

let solution: Types.Solution = { partA = partA; partB = partB }
