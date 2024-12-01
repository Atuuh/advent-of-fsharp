module AOC.Year2023.Day9

open AOC

let parseInput = String.split '\n' >> List.map (String.split ' ' >> List.map int64)

let integrate numbers =
    List.pairwise >> List.map (fun (a, b) -> b - a) <| numbers

let log message a =
    printfn "%s: %A" message a
    a

let integrateUntilZeroes numbers =
    let rec loop xs ys =
        if List.forall ((=) 0L) xs then
            ys
        else
            let integrated = integrate xs
            loop integrated (ys @ [ integrated ])

    loop numbers [ numbers ]

let predictPrevious xs =
    List.foldBack
        (fun x acc ->
            printfn "%i - %i: %i" x acc (x - acc)
            x - acc)
        xs
        0L

let private partA =
    parseInput
    >> Seq.map (integrateUntilZeroes >> List.map List.last >> List.sum)
    >> Seq.sum
    >> printfn "Result: %i"

let private partB =
    parseInput
    >> Seq.map (integrateUntilZeroes >> List.map List.head >> log "Head" >> predictPrevious)
    >> Seq.sum
    >> printfn "Result: %i"

let solution: Types.Solution = { partA = partA; partB = partB }
