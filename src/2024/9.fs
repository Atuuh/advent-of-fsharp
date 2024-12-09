module AOC.Year2024.Day9

open AOC

type X =
    | File of id: int * size: int
    | Empty of position: int * size: int

let isFile (block: X) = block.IsFile

// Take a size of space to fill and a list of files.
// Return a list of files to fill the space with and the remaining list of files
let replaceEmpty size files =
    files
    |> List.fold
        (fun (resultFiles, remainingSize, remainingFiles) file ->
            match file with
            | File _ when remainingSize = 0 -> resultFiles, 0, file :: remainingFiles
            | File(id, size) when size < remainingSize ->
                File(id, size) :: resultFiles, remainingSize - size, remainingFiles
            | File(id, size) when size > remainingSize ->
                File(id, remainingSize) :: resultFiles, 0, File(id, size - remainingSize) :: remainingFiles
            | File(id, size) when size = remainingSize -> File(id, remainingSize) :: resultFiles, 0, remainingFiles
            | _ -> failwith "Called replaceEmpty with some dumb shit")
        ([], size, [])
    |> (fun (a, _, c) -> a, List.rev c)

let blocksToStuff blocks =
    blocks
    |> List.mapi (fun i item ->
        match item with
        | File(id, size) ->
            [ for _ in i .. i + size - 1 do
                  string id ]
        | Empty(pos, size) ->
            [ for _ in i .. i + size - 1 do
                  "." ])
    |> List.flat

let printBlocks blocks msg =
    blocksToStuff blocks |> String.concat " " |> printfn "%s: [ %s ]" msg

let private partA input =
    let paddedInput = if String.length input % 2 = 0 then input else input + "0"
    printfn "oInput: %s\npInput: %s" input paddedInput

    let fileSystem =
        paddedInput
        |> String.splitEmpty
        |> List.fold
            (fun (s, i) item ->
                if i % 2 = 0 then
                    File(i / 2, int item) :: s, i + 1
                else
                    Empty(i, int item) :: s, i + 1)
            ([], 0)
        |> fst
        |> List.rev

    // printfn "Filesystem %A" fileSystem
    printBlocks fileSystem "File System"
    let filelength = blocksToStuff fileSystem |> List.filter ((=) ".") |> List.length
    printfn "filelength: %i" filelength

    let files = fileSystem |> List.filter isFile
    let emptys = fileSystem |> List.filter (isFile >> not)

    // For each Empty, replace it with enough files to fit
    let defragged =
        List.zip files emptys
        |> List.fold
            (fun (s, remainingFiles) (a, b) ->
                match b with
                | Empty(pos, size) ->
                    let insertFiles, remaining = replaceEmpty size remainingFiles
                    printfn "Insert files %A, remaining files %A" insertFiles (List.truncate 5 remaining)
                    List.concat [ List.ofSeq insertFiles; [ a ]; s ], remaining
                | _ -> failwith "We filtered this, right?")
            ([], List.rev files)
        |> fst
        |> List.rev


    let totalFileSize =
        files
        |> List.fold
            (fun size item ->
                match item with
                | File(id, s) -> size + s
                | Empty _ -> size)
            0

    printfn "Total size: %i" totalFileSize

    let kept =
        defragged
        |> List.fold
            (fun (result, totalSize) item ->
                match item with
                | File(id, size) when totalSize + size < totalFileSize -> item :: result, totalSize + size
                | File(id, size) when totalSize < totalFileSize ->
                    File(id, totalFileSize - totalSize) :: result, totalSize + size
                | File _ -> result, totalSize
                | Empty _ -> result, totalSize)
            ([], 0)
        |> fst
        |> List.rev

    // printfn "kept: %A" kept
    // kept |> Seq.iter (printfn "%A")

    let blocks =
        kept
        |> List.mapi (fun i item ->
            match item with
            | File(id, size) ->
                [ for j in i .. i + size - 1 do
                      uint64 id ])
        |> List.flat

    // blocks |> List.map string |> String.concat " " |> printfn "Defragged [ %s ]"
    printBlocks kept "Defragged"

    let getI index size =
        let length =
            seq {
                for i in index .. index + size - 1 do
                    i
            }
            |> Seq.sum

        // printfn "getI pos %i size %i = %i" index size length

        length

    let checksum = blocks |> List.mapi (fun i item -> uint64 i * item) |> List.sum
    printfn "Dumb checksum: %i" checksum

    let smartChecksum =
        kept
        |> Seq.fold
            (fun (checksum, blockPosition) item ->
                match item with
                | File(id, size) ->
                    // printfn "getI pos %i size %i = %i * fileId %i = %i" blockPosition size part id (part * id)
                    checksum + uint64 (getI blockPosition size * id), blockPosition + size
                | Empty(id, size) -> checksum, blockPosition + size)
            (0UL, 0)
        |> fst

    printfn "Smart checksum: %i" smartChecksum

    printfn "Answer: %i" checksum

let private partB = ignore

let solution: Types.Solution = { partA = partA; partB = partB }
