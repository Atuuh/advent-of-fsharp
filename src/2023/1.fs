module AOC.Year2023.Day1

open AOC
open System
open System.Text.RegularExpressions

let numberRegex = "[0-9]"

let private getFirstNumber value = Regex(numberRegex).Match(value).Value

let private getLastNumber value =
    Regex(numberRegex, RegexOptions.RightToLeft).Match(value).Value

let stringNumberRegex = "[0-9]|one|two|three|four|five|six|seven|eight|nine"

let private getTextFirstNumber value =
    Regex(stringNumberRegex).Match(value).Value

let private getTextLastNumber value =
    Regex(stringNumberRegex, RegexOptions.RightToLeft).Match(value).Value

let stringToNumber value =
    match value with
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9" -> value
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | _ -> raise (ArgumentException "Oops")

let private partA =
    Input.toList '\n'
    >> Seq.map (fun x -> [ getFirstNumber x; getLastNumber x ])
    >> Seq.map (fun x -> String.Join("", x))
    >> Seq.map int
    >> Seq.sum
    >> printfn "%i"

let private partB =
    Input.toList '\n'
    >> Seq.map (fun x -> [ getTextFirstNumber x; getTextLastNumber x ])
    >> Seq.map (fun x -> Seq.map stringToNumber x)
    >> Seq.map (fun x -> String.Join("", x))
    >> Seq.map int
    >> Seq.sum
    >> printfn "%i"

let solution: Types.Solution = { partA = partA; partB = partB }
