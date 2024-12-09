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

let private partA input =
    let files, empties = parseInput input
    let defragged = blockDefrag files empties
    let answer = checksum defragged
    printfn "Answer: %i" answer

let fileDefrag (files: File list) (empties: Empty list) =
    let rec loop files (empties: Empty list) (toCheck: File list) =
        match toCheck with
        | [] -> files
        | head :: tail ->
            let (id, size, position) = head

            match List.tryFind (fun (emptySpace, emptyPos) -> emptySpace >= size && emptyPos <= position) empties with
            | Some empty ->
                let (emptySpace, emptyPosition) = empty
                let remainingEmpties = List.filter ((=) empty >> not) empties
                let filesss = List.filter ((=) head >> not) files
                let newEmpty = Empty(emptySpace - size, emptyPosition + size)
                let updatedFile = File(id, size, emptyPosition)
                loop (updatedFile :: filesss) ((newEmpty :: remainingEmpties) |> List.sortBy snd) tail

            | None -> loop files empties tail

    loop files empties (List.rev files) |> List.sortBy thd3

let private partB input =
    let files, empties = parseInput input
    let defragged = fileDefrag files empties
    let answer = checksum defragged
    printfn "Answer: %i" answer

let solution: Types.Solution = { partA = partA; partB = partB }
