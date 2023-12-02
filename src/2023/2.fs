module AOC.Year2023.Day2

open System
open AOC
open AOC.String
open System.Text.RegularExpressions

let parseDiceCount compareFn value (r, g, b) =
    match value with
    | ParseRegex "(\d+) red" [ Integer red ] -> (compareFn r red, g, b)
    | ParseRegex "(\d+) green" [ Integer green ] -> (r, compareFn g green, b)
    | ParseRegex "(\d+) blue" [ Integer blue ] -> (r, g, compareFn b blue)
    | _ -> (r, g, b)

let getMaxColour value =
    Regex.Split(value, "[,;]")
    |> Seq.fold (fun x next -> parseDiceCount max next x) (0, 0, 0)

let hasAtleastRGB (r1, g1, b1) (r2, g2, b2) = r1 >= r2 && g1 >= g2 && b1 >= b2

let parseInput value =
    let result = Regex("Game (\d+): (.*)").Match(value)

    match Seq.toList result.Groups.Values with
    | [ _; first; second ] -> (first.Value, second.Value)
    | _ -> raise (ArgumentException "Oops")

let private partA =
    Input.toList '\n'
    >> Seq.map (parseInput >> (fun (id, revealed) -> (int id, getMaxColour revealed)))
    >> Seq.filterBy snd (hasAtleastRGB (12, 13, 14))
    >> Seq.map fst
    >> Seq.sum
    >> printfn "Winning game sums: %i"

let private partB =
    Input.toList '\n'
    >> Seq.map (
        parseInput
        >> (fun (_, revealed) -> getMaxColour revealed)
        >> (fun (a, b, c) -> a * b * c)
    )
    >> Seq.sum
    >> printfn "Winning minimum games: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
