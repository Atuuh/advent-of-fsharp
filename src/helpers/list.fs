module AOC.List

let flat list =
    List.fold (fun a b -> a @ b) List.empty list

let flatMap mapFn list = List.map mapFn list |> flat

let rec transpose xs =
    [ match xs with
      | [] -> failwith "Cannot transpose a 0-by-n matrix"
      | [] :: xs -> ()
      | xs ->
          yield List.map List.head xs
          yield! transpose (List.map List.tail xs) ]

let getElementCountMap xs =
    xs |> List.groupBy id |> List.map (fun (a, b) -> a, List.length b) |> Map.ofList
