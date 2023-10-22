module AOC.List


let flat list =
    List.fold (fun a b -> a @ b) List.empty list

let flatMap mapFn list = List.map mapFn list |> flat
