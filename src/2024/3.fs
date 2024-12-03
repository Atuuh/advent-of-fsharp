module AOC.Year2024.Day3

open System.Text.RegularExpressions
open AOC

let matchToPairs (m: Match) =
    let groups = m.Groups |> List.ofSeq |> List.map (fun group -> group.Value)
    
    match groups with 
    | [_;x;y] -> int x, int y
    | _ -> failwith (sprintf "Couldn't parse input, %A" groups)

let test text = 
    let matches = Regex.Matches(text, "mul\((\d{1,3}),(\d{1,3})\)")
    let pairs = List.ofSeq matches |> List.map matchToPairs
    List.fold (fun acc (x,y)  -> acc + x * y ) 0 pairs

let private partA input=
    let answer = test input
    printfn "Answer: %i" answer

let private partB =
    ignore

let solution: Types.Solution = { partA = partA; partB = partB }


