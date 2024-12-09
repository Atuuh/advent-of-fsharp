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
    |> List.fold
        (fun (total, blockPosition) (id, size, pos) ->
            total + fileChecksum (id, size) (uint64 blockPosition), blockPosition + size)
        (0UL, 0)
    |> fst

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

let private partA input =
    let files, empties = parseInput input
    let defragged = blockDefrag files empties
    let answer = checksum defragged
    printfn "Answer: %i" answer

let fileDefrag files empties =
    // let rec loop result filesToTry emptiesToTry =
    //     match filesToTry  with
    //     | [] -> result
    //     | (id, size)::tail ->
    //         let
    // loop [] (List.rev files) empties
    files

let private partB input =
    let files, empties = parseInput input
    let defragged = fileDefrag files empties
    let answer = checksum defragged
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
