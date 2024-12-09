module AOC.Year2024.Day9

open AOC
open AOC.Tuple

type File = int * int * int
type Empty = int * int

let replaceEmpty ((fullSize, pos): Empty) (files: File list) =
    files
    |> List.fold
        (fun (resultFiles, remainingSize, remainingFiles) (id, size, filePos) ->
            if remainingSize = 0 then
                resultFiles, 0, File(id, size, filePos) :: remainingFiles
            elif size < remainingSize then
                File(id, size, fullSize - remainingSize + pos) :: resultFiles, remainingSize - size, remainingFiles
            elif size > remainingSize then
                File(id, remainingSize, fullSize - remainingSize + pos) :: resultFiles,
                0,
                File(id, size - remainingSize, filePos) :: remainingFiles
            else
                File(id, remainingSize, fullSize - remainingSize + pos) :: resultFiles, 0, remainingFiles)
        ([], fullSize, [])
    |> (fun (a, _, c) -> a, List.rev c)

let inline fileChecksum (id: int, size: int) (position: uint64) : uint64 =
    seq {
        for i in position .. position + (uint64 size) - 1UL do
            i
    }
    |> Seq.sum
    |> (*) (uint64 id)

let checksum (files: File list) =
    files
    |> List.fold (fun (total) (id, size, pos) -> total + fileChecksum (id, size) (uint64 pos)) (0UL)
// |> fst

let parseInput input =
    let paddedInput = if String.length input % 2 = 0 then input else input + "0"

    let (filesReversed: File list), (emptiesReversed: Empty list), _, _ =
        paddedInput
        |> String.splitEmpty
        |> List.fold
            (fun (files, empties, i, diskPosition) item ->
                if i % 2 = 0 then
                    File(i / 2, int item, diskPosition) :: files, empties, i + 1, diskPosition + int item
                else
                    files, Empty(int item, diskPosition) :: empties, i + 1, diskPosition + int item)
            ([], [], 0, 0)

    let files = filesReversed |> List.rev
    let emptys = emptiesReversed |> List.rev
    files, emptys

let blockDefrag (files: File list) (empties: Empty list) : File list =
    let defragged =
        List.zip files empties
        |> List.fold
            (fun (s, remainingFiles) (a, emptySize) ->
                let insertFiles, remaining = replaceEmpty emptySize remainingFiles
                List.concat [ List.ofSeq insertFiles; [ a ]; s ], remaining)
            ([], List.rev files)
        |> fst
        |> List.rev


    let totalFileSize = files |> List.sumBy snd3

    defragged
    |> List.fold
        (fun (result, totalSize) (id, size, pos) ->
            if totalSize + size < totalFileSize then
                File(id, size, pos) :: result, totalSize + size
            elif totalSize < totalFileSize then
                File(id, totalFileSize - totalSize, pos) :: result, totalSize + size
            else
                result, totalSize)
        ([], 0)
    |> fst
    |> List.rev

let blockDefrag2 files empties =
    // let rec loop empties remainingFiles =

    // let newFiles = loop empties (List.rev files)
    // empties |> List.fold (fun s (size, pos) -> s) ([])
    ()

let private partA input =
    let files, empties = parseInput input
    let defragged = blockDefrag files empties
    let answer = checksum defragged
    printfn "Answer: %i" answer

let removeFirst predicate list =
    let rec loop acc =
        function
        | [] -> List.rev acc
        | h :: t when predicate h -> (List.rev acc) @ t
        | h :: t -> loop (h :: acc) t

    loop [] list

let fileDefrag (files: File list) (empties: Empty list) =

    let emptyPred size position (s, p) = size <= s && p < position

    let rec loop result filesToTry emptiesToTry =
        match filesToTry with
        | [] ->
            // printfn "Finished defragging"
            result
        | (id, size, position) :: tail ->
            // printfn
            //     "\nAttempting to defrag file %A\nempties %A\nresults %A"
            //     (File(id, size, position))
            //     emptiesToTry
            //     result

            match List.tryFind (emptyPred size position) emptiesToTry with
            | Some(emptySize, emptyPosition) ->
                let remainingEmpties = removeFirst (emptyPred size position) emptiesToTry
                let newEmpty = Empty(emptySize - size, emptyPosition + size)
                // printfn "Moving file %i to %A" id emptyPosition
                // printfn "Created new empty %A" newEmpty
                loop (File(id, size, emptyPosition) :: result) tail (newEmpty :: remainingEmpties)
            | None ->
                // printfn "Cannot defrag file %i" id
                loop result tail emptiesToTry

    let movedFiles = loop [] (List.rev files) empties
    // printfn "moved files %A" movedFiles

    let filesWithoutMovedFiles =
        List.filter (fun (a, _, _) -> List.exists (fst3 >> (=) a) movedFiles |> not) files

    // printfn "filesWithoutMovedFiles %A" filesWithoutMovedFiles

    filesWithoutMovedFiles @ movedFiles |> List.sortBy thd3

let print (files: File list) =
    files
    |> List.fold
        (fun (acc, lastPos) (id, size, pos) ->
            acc
            + ([ for i in lastPos .. pos - 1 do
                     if lastPos < pos then
                         "." ]
               |> String.concat "")
            + ([

                 for j in pos .. pos + size - 1 do
                     string id ]
               |> String.concat ""),
            pos + size)
        ("", 0)
    |> fst
    |> printfn "Defragged: [ %s ]"

let private partB input =
    let files, empties = parseInput input
    let defragged = fileDefrag files empties
    // printfn "defragged: %A" defragged
    // print defragged
    let answer = checksum defragged
    printfn "Answer: %i" answer

// printfn
//     "Fake checksum %i"
//     (checksum
//         [ File(0, 2, 0)
//           File(9, 2, 2)
//           File(2, 1, 5)
//           File(1, 4, 6)
//           File(7, 3, 9)
//           File(4, 2, 13)
//           File(3, 3, 16)
//           File(5, 4, 23)
//           File(6, 4, 28)
//           File(8, 4, 37) ])

let solution: Types.Solution = { partA = partA; partB = partB }
