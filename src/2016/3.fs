module AOC.Year2016.Day3

open AOC
open AOC.List

type Triangle = int * int * int

let isValidTriangle ((a, b, c): Triangle) = a + b > c && a + c > b && b + c > a

let stringSplit (separator: char) (value: string) =
    value.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList

let stringTrim (value: string) = value.Trim()

let log a =
    printfn "%A" a
    a

let rec transpose xs =
    [ match xs with
      | [] -> failwith "Cannot transpose a 0-by-n matrix"
      | [] :: xs -> ()
      | xs ->
          yield List.map List.head xs
          yield! transpose (List.map List.tail xs) ]

let mapInputToTriangle: string list -> Triangle =
    List.map int >> (fun (a: int list) -> (a[0], a[1], a[2]))

let partA =
    Input.mapToList (stringSplit ' ' >> mapInputToTriangle) '\n'
    >> List.filter isValidTriangle
    >> List.length
    >> printfn "%i triangles are possible"

let partB =
    Input.mapToList (stringSplit ' ') '\n'
    >> List.chunkBySize 3
    >> List.map transpose
    >> flat
    >> List.map (mapInputToTriangle)
    >> List.filter isValidTriangle
    >> List.length
    >> printfn "%i triangles are possible"

let solution: Types.Solution = { partA = partA; partB = partB }
