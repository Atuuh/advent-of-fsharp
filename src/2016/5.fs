module Year2016.Day5

open AOC

let log a =
    printfn "%A" a
    a

let swap fn a b = fn b a

let stringToInt32 (base_: int) value = System.Convert.ToInt32(value, base_)

let hash (value: string) =
    let md5 = System.Security.Cryptography.MD5.Create()

    System.Text.Encoding.UTF8.GetBytes(value)
    |> md5.ComputeHash
    |> Seq.map (fun x -> x.ToString("x2"))
    |> String.concat ""

let inline (>=<) a (b, c) = a >= b && a <= c

let checkFnA (value: string) = value.StartsWith "00000"

let checkFnB =
    let mutable (gotIndexes: int list) = List.empty

    let check (value: string) : bool =
        if checkFnA value then
            let index = System.Convert.ToInt32(value[5].ToString(), 16)

            if List.contains index gotIndexes then
                false
            elif index >=< (0, 7) then
                gotIndexes <- index :: gotIndexes
                true
            else
                false
        else
            false

    check

let doWork checkFn length (input: string) =
    let rec loop value nonce =
        let hashed = hash <| $"{value}{nonce}"

        if checkFn hashed then
            (hashed, nonce)
        else
            loop value (nonce + 1)

    [ 0 .. length - 1 ]
    |> List.fold
        (fun list item ->
            let last = List.last list
            let result = loop input (snd last + 1)
            list @ [ result ])
        [ (input, 0) ]
    |> List.tail

// let newDoWork checkFn length (input:string) =
//     let test i =
//         let hashed = hash <| $"{input}{i}"
//         if checkFn hashed then
//             (hashed, i)
//         else
//             ignore

//     Seq.initInfinite test |> Seq.take length

let partA =
    doWork checkFnA 8
    >> List.map (fun (x, _) -> x[5].ToString())
    >> String.concat ""
    >> printfn "The door code is %s"

let stringSet (value: string) index (input: string) =
    String.concat "" [ input[0 .. index - 1]; value; input[index + 1 ..] ]

let partB =
    doWork checkFnB 8
    >> List.map (fun (x, _) -> (System.Convert.ToInt32(x[5].ToString(), 16), x[6].ToString()))
    >> log
    >> List.fold (fun (res: string) (index, value) -> stringSet value index res) (String.replicate 8 "_")
    >> printfn "The door code is %s"

let solution: Types.Solution = { partA = partA; partB = partB }
