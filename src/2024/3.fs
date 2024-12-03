module AOC.Year2024.Day3

open System.Text.RegularExpressions
open AOC

type Command =
    | Mul of int * int
    | Do
    | Dont

let matchToMult (m: Match) =
    let groups = m.Groups |> List.ofSeq |> List.map (fun group -> group.Value)

    match groups with
    | [ _; x; y ] -> Mul(int x, int y)
    | _ -> failwith (sprintf "Couldn't parse input, %A" groups)

let parseCommand regex (mapping: Match -> Command) input =
    Regex.Matches(input, regex)
    |> List.ofSeq
    |> List.map (fun m -> m.Index, mapping m)

let getCommands input =
    let mults = parseCommand "mul\((\d{1,3}),(\d{1,3})\)" matchToMult input
    let dos = parseCommand "do\(\)" (fun _ -> Do) input
    let donts = parseCommand "don't\(\)" (fun _ -> Dont) input

    List.concat [ mults; dos; donts ] |> List.sortBy (fun (index, _) -> index)

let removeDisabledCommands (commands: list<int * Command>) =
    commands
    |> List.fold
        (fun (coms, enabled) item ->
            match (snd item), enabled with
            | (Mul _), true -> item :: coms, true
            | (Mul _), false -> coms, false
            | (Do), _ -> coms, true
            | (Dont), _ -> coms, false

        )
        ([], true)
    |> fst

let getMulAnswers commands =
    commands
    |> List.fold
        (fun acc (x, y) ->
            acc
            + match y with
              | Mul(a, b) -> a * b
              | _ -> 0)
        0

let private partA input =
    let commands = input |> getCommands
    let answer = getMulAnswers commands
    printfn "Answer: %i" answer

let private partB input =
    let commands = input |> getCommands |> removeDisabledCommands
    let answer = getMulAnswers commands
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
