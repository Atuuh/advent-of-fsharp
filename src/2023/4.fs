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

let folder (cards: (int * int) list) ((cardId, points): int * int) : (int * int) list =
    let cardCount = cards |> List.find (fst >> (=) cardId) |> snd
    printfn "card %i cardCount %i" cardId cardCount

    cards
    |> List.map (fun (id, count) ->
        if id > cardId && id <= cardId + points then
            (id, count + cardCount)
        else
            (id, count))

let getWinningCards (cards: (int * int) list) =
    let state = List.map (fun (id, _) -> id, 1) cards
    printfn "getWinningCards state %A" state
    List.fold folder state cards

let private partB =
    Input.mapToList parseInput '\n'
    >> List.map (
        fun (id, winning, numbers) -> id, List.where (fun x -> List.contains x winning) numbers
        >> fun (id, numbers) -> id, numbers.Length
    )
    >> getWinningCards
    >> List.sumBy snd
    >> printfn "Stuff: %A"

let solution: Types.Solution = { partA = partA; partB = partB }
