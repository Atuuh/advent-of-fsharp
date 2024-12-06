module AOC.Direction

open AOC.Point

type Direction =
    | N
    | E
    | S
    | W
    | NE
    | SE
    | SW
    | NW

let getVectorFromDirection direction =
    match direction with
    | N -> { X = 0; Y = -1 }
    | NE -> { X = 1; Y = -1 }
    | E -> { X = 1; Y = 0 }
    | SE -> { X = 1; Y = 1 }
    | S -> { X = 0; Y = 1 }
    | SW -> { X = -1; Y = 1 }
    | W -> { X = -1; Y = 0 }
    | NW -> { X = -1; Y = -1 }
