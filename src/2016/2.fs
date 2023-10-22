module Year2016.Day2

exception Error of string

let move direction (position: int) =
    match direction with
    | 'L' when List.contains position [ 1; 4; 7 ] -> position
    | 'L' -> position - 1
    | 'R' when List.contains position [ 3; 6; 9 ] -> position
    | 'R' -> position + 1
    | 'U' when List.contains position [ 1; 2; 3 ] -> position
    | 'U' -> position - 3
    | 'D' when List.contains position [ 7; 8; 9 ] -> position
    | 'D' -> position + 3
    | _ -> raise (Error("Direction is fucked!"))



let private partA = ignore
let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
