module AOC.Year2016.Day3

open AOC
open AOC.List

type Triangle = int * int * int
exception Error of string

let isValidTriangle ((a, b, c): Triangle) = a + b > c && a + c > b && b + c > a

let stringTrim (value: string) = value.Trim()

let log a =
    printfn "%A" a
    a

let mapInputToTriangle: string list -> Triangle =
    List.map int
    >> (fun (items: int list) ->
        match items with
        | [ a; b; c ] -> a, b, c
        | _ -> raise (Error("Not a list of 3 ints")))

let partA =
    Input.mapToList (String.split ' ' >> mapInputToTriangle) '\n'
    >> List.filter isValidTriangle
    >> List.length
    >> printfn "%i triangles are possible"

let partB =
    Input.mapToList (String.split ' ') '\n'
    >> List.chunkBySize 3
    >> List.map transpose
    >> flat
    >> List.map (mapInputToTriangle)
    >> List.filter isValidTriangle
    >> List.length
    >> printfn "%i triangles are possible"

let solution: Types.Solution = { partA = partA; partB = partB }
