module AOC.Year2023.Day7

open AOC
open Day5
open AOC.Tuple

let log message a =
    printfn "%s: %A" message a
    a

let getCardValue card =
    match card with
    | "2" -> 1
    | "3" -> 2
    | "4" -> 3
    | "5" -> 4
    | "6" -> 5
    | "7" -> 6
    | "8" -> 7
    | "9" -> 8
    | "T" -> 10
    | "J" -> 11
    | "Q" -> 12
    | "K" -> 13
    | "A" -> 14
    | _ -> raise (System.ArgumentException "Inputs wrong!")

let getCardValueWithJoker card =
    match card with
    | "J" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    | "6" -> 6
    | "7" -> 7
    | "8" -> 8
    | "9" -> 9
    | "T" -> 10
    | "Q" -> 12
    | "K" -> 13
    | "A" -> 14
    | _ -> raise (System.ArgumentException "Inputs wrong!")

let getHandPoints =
    List.groupBy id
    >> (List.map (snd >> List.length))
    >> (List.sortDescending)
    >> fun stuff ->
        match stuff with
        | [ 5 ] -> 7
        | [ 4; _ ] -> 6
        | [ 3; 2 ] -> 5
        | [ 3; _; _ ] -> 4
        | [ 2; 2; _ ] -> 3
        | [ 2; _; _; _ ] -> 2
        | _ -> 1

let parseInput =
    String.split '\n'
    >> List.map (fun line ->
        let [ cards; bid ] = String.split ' ' line
        String.splitEmpty cards, int bid)

let getCardPoints cardValueFn cards =
    List.mapi
        (fun i card ->
            let cardValue = cardValueFn card

            let shift =
                match i with
                | 0 -> 0x010000
                | 1 -> 0x001000
                | 2 -> 0x000100
                | 3 -> 0x000010
                | 4 -> 0x000001

            cardValue * shift)
        cards

let testes cardValueFn (cards, bid) =
    let handPoints = getHandPoints cards * 0x100000

    let cardPoints =
        List.mapi
            (fun i card ->
                let cardValue = cardValueFn card

                let shift =
                    match i with
                    | 0 -> 0x010000
                    | 1 -> 0x001000
                    | 2 -> 0x000100
                    | 3 -> 0x000010
                    | 4 -> 0x000001

                cardValue * shift)
            cards
        |> List.sum

    cards, handPoints + cardPoints, bid

let private partA =
    parseInput
    >> List.map (testes getCardValue)
    >> List.sortBy snd3
    >> List.map thd3
    >> List.mapi (fun i bid -> (i + 1) * bid)
    >> List.sum
    >> printfn "Winnings: %i"

let getBonusHands cards =
    let cardTypes = List.distinct cards
    let jokerCount = cards |> List.filter ((=) "J") |> List.length

    let rec loop hands fromIndex : string list list =
        if fromIndex >= 5 then
            if List.length hands <> (pown (List.length cardTypes) jokerCount) then
                raise (System.ArgumentException "Got wrong amount of bonus hands")
            else
                hands
        else
            let bonusHands =
                hands
                |> List.choose (fun hand ->
                    if List.item fromIndex hand = "J" then
                        Some(cardTypes |> List.map (fun newCard -> List.updateAt fromIndex newCard hand))
                    else
                        None)
                |> List.collect id

            if List.isEmpty bonusHands then
                loop hands (fromIndex + 1)
            else
                loop (bonusHands) (fromIndex + 1)

    if List.contains "J" cardTypes then
        loop (List.singleton cards) 0
    else
        List.Empty

let private partB =
    parseInput
    >> List.map (fun (cards, bid) ->
        let bonusHands = cards |> getBonusHands

        let allHands = (cards :: bonusHands)

        let handScore = allHands |> List.map getHandPoints |> List.max |> (*) 0x100000

        let cardScore = getCardPoints getCardValueWithJoker cards |> List.sum


        cards, handScore + cardScore, bid)
    >> List.sortBy snd3
    >> List.map thd3
    >> List.mapi (fun i bid -> (i + 1) * bid)
    >> List.sum
    >> printfn "Winnings: %i"


let solution: Types.Solution = { partA = partA; partB = partB }
