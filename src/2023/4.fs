module AOC.Year2023.Day4

open System
open AOC
open System.Text.RegularExpressions

let parseInput input = 
    let result = Regex("Card.+?(\d+):(.+)").Match(input)

    let id = (result.Groups.Item 1).Value |> int
    let getNumbers value = String.split '|' >> List.map (String.split ' ' >> Seq.map int >> List.ofSeq) <| value
    
    match getNumbers (result.Groups.Item 2).Value with
    | [winningNumbers; numbers] -> (id, winningNumbers, numbers)
    | _ -> raise(ArgumentException "Oops")

let private partA =
   Input.mapToList parseInput '\n'
   >> List.map  (
        fun (id, winning, numbers) -> List.where (fun x -> List.contains x winning) numbers
        >> fun x -> pown 2  (x.Length - 1)
    )
   >> List.sum
   >> printfn "Total score: %i"

let private partB =
    ignore

let solution: Types.Solution = { partA = partA; partB = partB }
