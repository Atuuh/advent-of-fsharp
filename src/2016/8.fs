module Year2016.Day8

open AOC
open AOC.String

let rotate (amount: int) (a: 'a array) =
    let len = Array.length a
    let actualAmount = len - amount % len
    Array.append a[actualAmount..] a[.. actualAmount - 1]

let screen: int[,] = Array2D.zeroCreate 50 6

let rect x y (arr: int[,]) =
    let newArr = Array2D.copy arr

    for i = 0 to x - 1 do
        for j = 0 to y - 1 do
            newArr[i, j] <- 1

    newArr

let rotateRow row amount (arr: int[,]) =
    let newArr = Array2D.copy arr
    let targetRow = arr[*, row]
    let rotated = rotate (amount) targetRow

    for i = 0 to Array2D.length1 arr - 1 do
        newArr[i, row] <- rotated[i]

    newArr

let rotateCol col amount (arr: int[,]) =
    let newArr = Array2D.copy arr
    let targetCol = arr[col, *]
    let rotated = rotate (amount) targetCol

    for i = 0 to Array2D.length2 arr - 1 do
        newArr[col, i] <- rotated[i]

    newArr

let result = rotateRow 1 1 screen

let printScreen (a: int[,]) =
    for i = 0 to Array2D.length2 a - 1 do
        for j = 0 to Array2D.length1 a - 1 do
            printf "%s"
            <| match a[j, i] with
               | 0 -> "."
               | 1 -> "#"
               | _ -> ""

        printf "\n"

let parseLine str =
    printfn "parseLine %s" str

    match str with
    | ParseRegex "rect (\d+)x(\d+)" [ Integer x; Integer y ] -> rect x y
    | ParseRegex "rotate row y=(\d+) by (\d+)" [ Integer y; Integer amount ] -> rotateRow y amount
    | ParseRegex "rotate column x=(\d+) by (\d+)" [ Integer x; Integer amount ] -> rotateCol x amount
    | e -> raise (System.Exception($"Get fucked, {e}"))

let print a =
    for j = 0 to Array2D.length2 a - 1 do
        for i = 0 to Array2D.length1 a - 1 do
            match a[i, j] with
            | 0 -> printf "."
            | 1 -> printf "#"
            | _ -> printfn "Error"

        printf "\n"

let partA =
    Input.toList '\n'
    >> List.fold (fun acc str -> parseLine str acc) screen
    >> Seq.cast<int>
    >> Seq.filter (fun x -> x = 1)
    >> Seq.length
    >> printfn "%i"

let partB =
    Input.toList '\n'
    >> List.fold (fun acc str -> parseLine str acc) screen
    >> print

let solution: Types.Solution = { partA = partA; partB = partB }
