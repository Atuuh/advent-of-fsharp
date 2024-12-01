module AOC.Year2023.Day5

open AOC
open System

type Range = int64 * int64
type Table = (Range * Range) list

let intersectRange (a, al) (b, bl) =
    if a <= b && a + al >= b + bl then
        let lower = a, b - a
        let overlap = b, bl
        let upper = b + bl, (a + al) - (b + bl)
        [ lower; upper ] |> List.filter (snd >> ((<>) 0L)), overlap
    elif b <= a && b + bl >= a + al then
        [], (a, al)
    elif a < b && a + al < b + bl then
        let lower = a, b - a
        let overlap = b, a + al - b
        [ lower ], overlap
    elif a > b && a + al - 1L > b + bl - 1L then
        let upper = b + bl, (a + al) - (b + bl)
        let overlap = a, b + bl - a
        [ upper ], overlap
    else
        raise (ArgumentException "Something dun fucked")

let takeByI iFn =
    Seq.mapi (fun i item -> if iFn i then Some item else None) >> Seq.choose id

let isEven x = x % 2 = 0
let isOdd x = x % 2 <> 0

let getSeeds input : Range list =
    input
    |> ((String.split ':')
        >> (Seq.item 1)
        >> (String.split ' ')
        >> (Seq.map int64)
        >> Seq.map (fun x -> x, 1L)
        >> List.ofSeq)

let getSeedRanges input : Range list =
    input
    |> ((String.split ':')
        >> (Seq.item 1)
        >> (String.split ' ')
        >> (Seq.map int64)
        >> (fun xs -> Seq.zip (takeByI isEven xs) (takeByI isOdd xs))
        >> List.ofSeq)

let parseInput getSeedFn (value: string) =
    let liststs = value.Split("\n\n")

    let seeds = getSeedFn (Seq.head liststs)

    let tables =
        (Seq.tail liststs)
        |> (Seq.map<string, Table> (
                String.split '\n'
                >> Seq.tail
                >> Seq.map (
                    String.split ' '
                    >> fun xs ->
                        match xs with
                        | [ a; b; c ] -> int64 a, int64 b, int64 c
                        | _ -> raise (ArgumentException "Oopsie")
                    >> (fun (a, b, c) -> (b, c), (a, c))
                )
                >> Seq.sortBy (fst >> fst)
                >> List.ofSeq
            )
            >> List.ofSeq)

    (seeds, tables)

let mapRangesAgainstTable ranges table =
    let rec loop unmatchedRanges matchedRanges =
        match unmatchedRanges with
        | [] -> matchedRanges
        | (a, al) :: tail ->
            let mapping = List.tryFind (fun ((b, bl), _) -> a >= b && a <= b + bl - 1L) table

            match mapping with
            | None -> loop tail ((a, al) :: matchedRanges)
            | Some((b, bl), (t, tl)) ->
                let (newUnmatchedRanges, (m, ml)) = intersectRange (a, al) (b, bl)
                let diff = m - b
                loop (tail @ newUnmatchedRanges) ((t + diff, ml) :: matchedRanges)

    loop ranges []

let private partA =
    parseInput getSeeds
    >> (fun (seeds, tables) -> List.fold mapRangesAgainstTable seeds tables)
    >> List.map fst
    >> List.min
    >> printfn "Result: %i"

let private partB =
    parseInput getSeedRanges
    >> (fun (seeds, tables) -> List.fold mapRangesAgainstTable seeds tables)
    >> List.map fst
    >> List.min
    >> printfn "Result: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
