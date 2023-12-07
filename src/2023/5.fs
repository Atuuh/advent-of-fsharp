module AOC.Year2023.Day5

open AOC
open System

type Table = (int64 * int64 * int64) list

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thd3 (_, _, c) = c

let parseInput (value: string) : int64 list * Table list =
    let liststs = value.Split("\n\n")

    let seeds =
        (Seq.head liststs)
        |> ((String.split ':')
            >> (Seq.item 1)
            >> (String.split ' ')
            >> (Seq.map int64)
            >> List.ofSeq)

    let tables =
        (Seq.tail liststs)
        |> (Seq.map (
                String.split '\n'
                >> Seq.tail
                >> Seq.map (
                    String.split ' '
                    >> fun xs ->
                        match xs with
                        | [ a; b; c ] -> int64 a, int64 b, int64 c
                        | _ -> raise (ArgumentException "Oopsie")
                )
                >> List.ofSeq
            )
            >> List.ofSeq)

    (seeds, tables)



let lookupTable (table: Table) (value: int64) : int64 =
    let sortedTable = table |> List.sortByDescending snd3
    // printfn "Sorted: %A" sortedTable
    let lookup = sortedTable |> List.tryFind (fun (a, b, c) -> b <= value)
    // printfn "Lookup: %A" lookup

    lookup
    |> (fun opt ->
        match opt with
        | Some(a, b, c) ->
            // printfn "Matching %i between %i and %i\n" value b (b + c)

            if b <= value && b + c >= value then
                a + (value - b)
            else
                value
        | None ->
            // printfn "Nothing to match\n"
            value)

let private partA =
    parseInput
    >> fun (seeds, tables) ->
        tables
        |> List.fold
            (fun newSeeds table ->
                // printfn "%A" newSeeds
                newSeeds |> List.map (lookupTable table))
            seeds
    >> List.min
    >> printfn "%A"

let private partB = ignore


let solution: Types.Solution = { partA = partA; partB = partB }
