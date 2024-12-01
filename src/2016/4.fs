module AOC.Year2016.Day4

open AOC
open System
open System.Text.RegularExpressions

type Room = string * int * string
exception Error of string

let stringToRoom (value: string) =
    let m = Regex.Match(value, @"(.+)-(\d+)\[(.+)\]")

    match Seq.toList m.Groups.Values with
    | [ _; a; b; c ] -> (a.Value, int b.Value, c.Value)
    | a ->
        printfn "%A" a
        raise (Error("Regex failed"))

let log a =
    printfn "%A" a
    a

let snd3 (_, b, _) = b

let charToCharCode (char: Char) = (int char)

let validRoom ((name, _, checksum): Room) =
    String.split '-' name
    |> List.flatMap (fun x -> x.ToCharArray() |> Seq.toList)
    |> List.groupBy id
    |> List.sortBy (fun (a, b) -> a)
    |> List.sortByDescending (fun (a, b) -> b.Length)
    |> List.takeWhile (fun (a, _) -> checksum.Contains(a))
    |> List.length
    |> (=) 5

let swap fn a b = fn b a

let rotateLetter amount (letter: Char) =
    charToCharCode letter
    |> swap (-) (charToCharCode 'a')
    |> swap (+) amount
    |> swap (%) 26
    |> swap (+) (charToCharCode 'a')
    |> char

let decryptWord sector (word: string) =
    word.ToCharArray() |> Seq.map (rotateLetter sector) |> String.Concat

let partA =
    Input.mapToList stringToRoom '\n'
    >> List.filter validRoom
    >> List.map snd3
    >> List.sum
    >> printfn "Sum of sector IDs is %i"

let partB =
    Input.mapToList stringToRoom '\n'
    >> List.filter validRoom
    >> List.map (fun (name, sector, _) -> (decryptWord sector name), sector)
    >> List.where (fun (a, _) -> a.Contains "north")
    >> List.iter (printfn "%A")

let solution: Types.Solution = { partA = partA; partB = partB }
