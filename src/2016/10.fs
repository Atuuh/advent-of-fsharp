module AOC.Year2016.Day10

open AOC
open AOC.String

type Bot = int
type Output = int

type A =
    | Bot of bot: int
    | Output of output: int

type Transfer = int * A * A
type Input = int * int

type Command =
    | Input of value: int * bot: int
    | Transfer of bot: int * low: A * high: A

let parseRule (value: string) =
    match value with
    | ParseRegex "bot (\d+) gives low to bot (\d+) and high to bot (\d+)" [ Integer bot; Integer low; Integer high ] ->
        Transfer(bot, Bot(low), Bot(high))
    | ParseRegex "bot (\d+) gives low to output (\d+) and high to bot (\d+)" [ Integer bot; Integer low; Integer high ] ->
        Transfer(bot, Output(low), Bot(high))
    | ParseRegex "bot (\d+) gives low to bot (\d+) and high to output (\d+)" [ Integer bot; Integer low; Integer high ] ->
        Transfer(bot, Bot(low), Output(high))
    | ParseRegex "bot (\d+) gives low to output (\d+) and high to output (\d+)" [ Integer bot; Integer low; Integer high ] ->
        Transfer(bot, Output(low), Output(high))
    | ParseRegex "value (\d+) goes to bot (\d+)" [ Integer value; Integer bot ] -> Input(value, bot)
    | _ -> raise (System.Exception("Something dun goofed"))

let input =
    @"value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2"

let commands = Input.mapToList parseRule '\n' input

let initialState =
    List.fold
        (fun state command ->
            match command with
            | Input(value, bot) -> state
            | _ -> state)
        Map.empty<int, int * int>
        commands
// |> printfn "test input %A"

let partA = ignore

let partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
