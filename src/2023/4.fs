module AOC.Year2023.Day4

open System
open AOC
open System.Text.RegularExpressions

let parseInput input =
    let result = Regex("Card.+?(\d+):(.+)").Match(input)

    let id = (result.Groups.Item 1).Value |> int

    let getNumbers value =
        String.split '|' >> List.map (String.split ' ' >> Seq.map int >> List.ofSeq)
        <| value

    match getNumbers (result.Groups.Item 2).Value with
    | [ winningNumbers; numbers ] -> (id, winningNumbers, numbers)
    | _ -> raise (ArgumentException "Oops")

let private partA =
    Input.mapToList parseInput '\n'
    >> List.map (
        fun (id, winning, numbers) -> List.where (fun x -> List.contains x winning) numbers
        >> fun x -> pown 2 (x.Length - 1)
    )
    >> List.sum
    >> printfn "Total score: %i"


let getWinningCards (cards: (int * int list) list) =
    let getCard id = List.find (fun (i, _) -> i = id) cards

    let rec loop (remainingWinningCards: int list) (totalCards: int list) =
        if List.isEmpty remainingWinningCards then
            totalCards
        else
            let newWinningCards =
                remainingWinningCards
                |> List.collect (fun x ->
                    let c = getCard x
                    snd c)

            loop newWinningCards (totalCards @ newWinningCards)

    loop (List.map fst cards) (List.map fst cards)

let private partB =
    Input.mapToList parseInput '\n'
    >> List.map (
        fun (id, winning, numbers) -> id, List.where (fun x -> List.contains x winning) numbers
        >> fun (id, numbers) -> id, [ id + 1 .. 1 .. id + numbers.Length ]
    )
    >> getWinningCards
    // >> List.length
    >> List.sort
    >> List.length
    >> printfn "Stuff: %A"

let solution: Types.Solution = { partA = partA; partB = partB }
