module AOC.Year2023.Day1

open AOC
open System
open System.Text.RegularExpressions

let numberRegex = "[0-9]"
let stringNumberRegex = "[0-9]|one|two|three|four|five|six|seven|eight|nine"

let regexMatch pattern value = Regex(pattern).Match(value).Value

let regexMatchRTL pattern value =
    Regex(pattern, RegexOptions.RightToLeft).Match(value).Value

let private getFirstNumber = regexMatch numberRegex
let private getLastNumber = regexMatchRTL numberRegex
let private getTextFirstNumber = regexMatch stringNumberRegex
let private getTextLastNumber = regexMatchRTL stringNumberRegex

let stringToNumber value =
    match value with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | _ -> value

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
