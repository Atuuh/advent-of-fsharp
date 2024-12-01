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

let getWinningNumbers (id, winning, numbers) =
    id, List.where (fun x -> List.contains x winning) numbers

let private partA =
    Input.mapToList parseInput '\n'
    >> List.map (getWinningNumbers >> fun numbers -> pown 2 (snd numbers |> List.length |> (-) 1))
    >> List.sum
    >> printfn "Total score: %i"

let getWinningCards (cards: (int * int) list) =
    List.fold
        (fun cards (cardId, points) ->
            let cardCount = cards |> List.find (fst >> (=) cardId) |> snd

            cards
            |> List.map (fun (id, count) ->
                if id > cardId && id <= cardId + points then
                    (id, count + cardCount)
                else
                    (id, count)))
        (List.map (fun (id, _) -> id, 1) cards)
        cards

let private partB =
    Input.mapToList parseInput '\n'
    >> List.map (getWinningNumbers >> fun (id, numbers) -> id, numbers.Length)
    >> getWinningCards
    >> List.sumBy snd
    >> printfn "Stuff: %A"

let solution: Types.Solution = { partA = partA; partB = partB }
